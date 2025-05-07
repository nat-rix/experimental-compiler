pub mod instr;
pub mod ssa;

use instr::Instr;
use ssa::SsaBlock;

use crate::{
    error::SemanticError,
    parser::{
        ast::{AsgnOp, Ast, Expr, Ident, LValue, Lit, Op1, Op2, Stmt},
        span::{Spanned, SrcSpan},
    },
};

/// Abstract register.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AReg(pub usize);

#[derive(Debug, Clone, Copy)]
struct ARegAlloc(AReg);

impl Default for ARegAlloc {
    fn default() -> Self {
        Self(AReg(0))
    }
}

impl ARegAlloc {
    pub fn alloc(&mut self) -> AReg {
        let reg = self.0;
        self.0.0 += 1;
        reg
    }
}

#[derive(Debug, Clone)]
struct Variable<'a> {
    ident: Ident<'a>,
    span: SrcSpan,
    reg: Option<AReg>,
}

impl<'a> Variable<'a> {
    pub fn reg_or_alloc(&mut self, alloc: &mut ARegAlloc) -> &mut AReg {
        self.reg.get_or_insert_with(|| alloc.alloc())
    }

    pub fn reg(&self, span: &SrcSpan) -> Result<AReg, SemanticError<'a>> {
        self.reg.ok_or(SemanticError::UnassignedVariable {
            ident: self.ident,
            declaration: self.span,
            usage: *span,
        })
    }
}

#[derive(Debug, Clone, Default)]
struct Vars<'a> {
    vars: Vec<Variable<'a>>,
}

impl<'a> Vars<'a> {
    pub fn get<'b>(
        &'b mut self,
        ident: &Ident<'a>,
        span: &SrcSpan,
    ) -> Result<&'b mut Variable<'a>, SemanticError<'a>> {
        self.vars
            .iter_mut()
            .find(|var| &var.ident == ident)
            .ok_or(SemanticError::UndefinedVariable(*ident, *span))
    }

    pub fn find(&self, ident: &Ident<'a>) -> Option<&Variable<'a>> {
        self.vars.iter().find(|var| &var.ident == ident)
    }

    pub fn define(
        &mut self,
        ident: Ident<'a>,
        span: SrcSpan,
        alloc: impl FnOnce() -> Option<AReg>,
    ) -> Result<(), SemanticError<'a>> {
        if let Some(var) = self.find(&ident) {
            return Err(SemanticError::VariableRedefinition {
                ident,
                definition1: var.span,
                definition2: span,
            });
        }
        self.vars.push(Variable {
            ident,
            span,
            reg: alloc(),
        });
        Ok(())
    }
}

#[derive(Debug, Clone, Default)]
pub struct CodeGen<'a> {
    code: Vec<Instr<AReg>>,
    vars: Vars<'a>,
    reg_alloc: ARegAlloc,
}

impl<'a> CodeGen<'a> {
    pub fn from_ast(ast: &Spanned<Ast<'a>>) -> Result<Self, SemanticError<'a>> {
        let mut slf = Self::default();
        slf.generate(ast)?;
        Ok(slf)
    }

    pub fn generate(&mut self, ast: &Spanned<Ast<'a>>) -> Result<(), SemanticError<'a>> {
        for stmt in ast.val.iter_spanned() {
            if !self.generate_stmt(stmt)? {
                continue;
            }
            return Ok(());
        }
        Err(SemanticError::MissingReturn(ast.span))?
    }

    fn generate_stmt(&mut self, stmt: &Spanned<Stmt<'a>>) -> Result<bool, SemanticError<'a>> {
        match &stmt.val {
            Stmt::Decl(ident) => {
                self.vars.define(*ident, stmt.span, || None)?;
            }
            Stmt::Init(ident, expr) => {
                let dst = self.reg_alloc.alloc();
                self.generate_expr(expr, &stmt.span, dst)?;
                self.vars.define(*ident, stmt.span, || Some(dst))?;
            }
            Stmt::Asgn(LValue(ident), op, rhs) => {
                let f: Option<fn(_, _) -> _> = match op {
                    AsgnOp::Asg => None,
                    AsgnOp::Add => Some(Instr::Add),
                    AsgnOp::Sub => Some(Instr::Sub),
                    AsgnOp::Mul => Some(Instr::Mul),
                    AsgnOp::Div => Some(Instr::Div),
                    AsgnOp::Mod => Some(Instr::Mod),
                };
                if let Some(f) = f {
                    let tmp = self.reg_alloc.alloc();
                    self.generate_expr(rhs, &stmt.span, tmp)?;
                    let var = self.vars.get(ident, &stmt.span)?;
                    let reg = var.reg(&stmt.span)?;
                    self.code.push(f(reg, [reg, tmp]));
                } else {
                    let reg = *self
                        .vars
                        .get(ident, &stmt.span)?
                        .reg_or_alloc(&mut self.reg_alloc);
                    self.generate_expr(rhs, &stmt.span, reg)?;
                }
            }
            Stmt::Ret(expr) => {
                let dst = self.reg_alloc.alloc();
                self.generate_expr(expr, &stmt.span, dst)?;
                self.code.push(Instr::Return(dst));
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn generate_expr(
        &mut self,
        expr: &Expr<'a>,
        span: &SrcSpan,
        dst: AReg,
    ) -> Result<(), SemanticError<'a>> {
        match expr {
            Expr::Lit(Lit::Int(val)) => self.code.push(Instr::LoadConst(dst, *val)),
            Expr::Ident(ident) => {
                let reg = self.vars.get(ident, span)?.reg(span)?;
                self.code.push(Instr::Move(dst, reg));
            }
            Expr::Op1(Op1, expr) => {
                self.generate_expr(expr, span, dst)?;
                self.code.push(Instr::Neg(dst, dst));
            }
            Expr::Op2(op2, lhs, rhs) => {
                self.generate_expr(lhs, span, dst)?;
                let rhs_reg = self.reg_alloc.alloc();
                self.generate_expr(rhs, span, rhs_reg)?;
                let f = match op2 {
                    Op2::Add => Instr::Add,
                    Op2::Sub => Instr::Sub,
                    Op2::Mul => Instr::Mul,
                    Op2::Div => Instr::Div,
                    Op2::Mod => Instr::Mod,
                };
                self.code.push(f(dst, [dst, rhs_reg]));
            }
        }
        Ok(())
    }

    pub fn code(&self) -> &[Instr<AReg>] {
        &self.code
    }

    pub fn into_ssa(self) -> SsaBlock<AReg> {
        let Self {
            code,
            mut reg_alloc,
            ..
        } = self;
        SsaBlock::from_code(code, || reg_alloc.alloc())
    }
}

pub mod cprop;
pub mod instr;
pub mod precolorizer;
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
pub struct ARegAlloc(AReg);

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

    pub fn count(&self) -> usize {
        self.0.0
    }
}

#[derive(Debug, Clone)]
struct Variable<'a> {
    ident: Ident<'a>,
    span: SrcSpan,
    reg: Option<AReg>,
}

impl<'a> Variable<'a> {
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

struct Op2Wrap(Op2);

impl Op2Wrap {
    fn to_instr<T>(&self, d: T, s1: T, s2: T, alloc: impl FnOnce() -> T) -> Instr<T> {
        match self.0 {
            Op2::Add => Instr::AddRR(d, [s1, s2]),
            Op2::Sub => Instr::SubRR(d, [s1, s2]),
            Op2::Mul => Instr::IMulRR(d, [s1, s2]),
            Op2::Div => Instr::DivModRR([d, alloc()], [s1, s2]),
            Op2::Mod => Instr::DivModRR([alloc(), d], [s1, s2]),
        }
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
        let mut found_return = None;
        for stmt in ast.val.iter_spanned() {
            if self.generate_stmt(stmt)? && found_return.is_none() {
                found_return = Some(self.code.len());
            }
        }
        if let Some(len) = found_return {
            self.code.truncate(len);
            Ok(())
        } else {
            Err(SemanticError::MissingReturn(ast.span))?
        }
    }

    fn generate_stmt(&mut self, stmt: &Spanned<Stmt<'a>>) -> Result<bool, SemanticError<'a>> {
        match &stmt.val {
            Stmt::Decl(ident) => {
                self.vars.define(*ident, stmt.span, || None)?;
            }
            Stmt::Init(ident, expr) => {
                let dst = self.reg_alloc.alloc();
                let dst = self.generate_expr(expr, &stmt.span, Some(dst))?;
                self.vars.define(*ident, stmt.span, || Some(dst))?;
            }
            Stmt::Asgn(LValue(ident), op, rhs) => {
                let f = match op {
                    AsgnOp::Asg => None,
                    AsgnOp::Add => Some(Op2::Add),
                    AsgnOp::Sub => Some(Op2::Sub),
                    AsgnOp::Mul => Some(Op2::Mul),
                    AsgnOp::Div => Some(Op2::Div),
                    AsgnOp::Mod => Some(Op2::Mod),
                };
                if let Some(f) = f {
                    let tmp = self.generate_expr(rhs, &stmt.span, None)?;
                    let var = self.vars.get(ident, &stmt.span)?;
                    let reg = var.reg(&stmt.span)?;
                    let instr = Op2Wrap(f).to_instr(reg, reg, tmp, || self.reg_alloc.alloc());
                    self.code.push(instr);
                } else {
                    let var = self.vars.get(ident, &stmt.span)?;
                    let varreg = var.reg.unwrap_or_else(|| self.reg_alloc.alloc());
                    self.generate_expr(rhs, &stmt.span, Some(varreg))?;
                    self.vars.get(ident, &stmt.span)?.reg = Some(varreg);
                }
            }
            Stmt::Ret(expr) => {
                let dst = self.generate_expr(expr, &stmt.span, None)?;
                self.code.push(Instr::ReturnR(dst));
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn generate_expr(
        &mut self,
        expr: &Expr<'a>,
        span: &SrcSpan,
        dst: Option<AReg>,
    ) -> Result<AReg, SemanticError<'a>> {
        Ok(match expr {
            Expr::Lit(Lit::Int(val)) => {
                let dst = dst.unwrap_or_else(|| self.reg_alloc.alloc());
                self.code.push(Instr::MoveI(dst, val.0 as _));
                dst
            }
            Expr::Ident(ident) => {
                let reg = self.vars.get(ident, span)?.reg(span)?;
                if let Some(dst) = dst {
                    self.code.push(Instr::MoveR(dst, reg));
                    dst
                } else {
                    reg
                }
            }
            Expr::Op1(Op1, expr) => {
                let dst = dst.unwrap_or_else(|| self.reg_alloc.alloc());
                let dst = self.generate_expr(expr, span, Some(dst))?;
                self.code.push(Instr::NegR(dst, dst));
                dst
            }
            Expr::Op2(op2, lhs, rhs) => {
                let dst = dst.unwrap_or_else(|| self.reg_alloc.alloc());
                let lhs_reg = self.generate_expr(lhs, span, None)?;
                let rhs_reg = self.generate_expr(rhs, span, None)?;
                let instr =
                    Op2Wrap(*op2).to_instr(dst, lhs_reg, rhs_reg, || self.reg_alloc.alloc());
                self.code.push(instr);
                dst
            }
        })
    }

    pub fn code(&self) -> &[Instr<AReg>] {
        &self.code
    }

    pub fn into_ssa(self) -> SsaBlock<AReg> {
        SsaBlock::from_code(self.code, self.reg_alloc)
    }
}

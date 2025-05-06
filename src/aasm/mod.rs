use crate::{
    error::SemanticError,
    parser::{
        ast::{AsgnOp, Ast, Expr, Ident, IntLit, LValue, Lit, Op1, Op2, Stmt},
        span::{Spanned, SrcSpan},
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Register(pub usize);

#[derive(Debug, Clone)]
pub enum Instruction {
    LoadConst(Register, IntLit),
    Move(Register, Register),
    Neg(Register),
    Add(Register, Register),
    Sub(Register, Register),
    Mul(Register, Register),
    Div(Register, Register),
    Mod(Register, Register),
    Return(Register),
}

#[derive(Debug, Clone)]
struct Variable<'a> {
    ident: Ident<'a>,
    span: SrcSpan,
    reg: Register,
    assigned: bool,
}

#[derive(Debug, Clone, Default)]
pub struct CodeBlock<'a> {
    code: Vec<Instruction>,
    vars: Vec<Variable<'a>>,
    next_reg_id: usize,
}

impl<'a> CodeBlock<'a> {
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
                let reg = self.alloc_reg();
                self.alloc_var(*ident, stmt.span, reg, false)?;
            }
            Stmt::Init(ident, expr) => {
                let dst = self.alloc_reg();
                self.generate_expr(expr, &stmt.span, dst)?;
                self.alloc_var(*ident, stmt.span, dst, true)?;
            }
            Stmt::Asgn(LValue(ident), op, rhs) => {
                let f: Option<fn(_, _) -> _> = match op {
                    AsgnOp::Asg => None,
                    AsgnOp::Add => Some(Instruction::Add),
                    AsgnOp::Sub => Some(Instruction::Sub),
                    AsgnOp::Mul => Some(Instruction::Mul),
                    AsgnOp::Div => Some(Instruction::Div),
                    AsgnOp::Mod => Some(Instruction::Mod),
                };
                if let Some(f) = f {
                    let tmp = self.alloc_reg();
                    self.generate_expr(rhs, &stmt.span, tmp)?;
                    let reg = self.get_var(ident, &stmt.span, true)?.reg;
                    self.code.push(f(reg, tmp));
                } else {
                    let var = self.get_var(ident, &stmt.span, false)?;
                    var.assigned = true;
                    let reg = var.reg;
                    self.generate_expr(rhs, &stmt.span, reg)?;
                }
            }
            Stmt::Ret(expr) => {
                let dst = self.alloc_reg();
                self.generate_expr(expr, &stmt.span, dst)?;
                self.code.push(Instruction::Return(dst));
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn generate_expr(
        &mut self,
        expr: &Expr<'a>,
        span: &SrcSpan,
        dst: Register,
    ) -> Result<(), SemanticError<'a>> {
        match expr {
            Expr::Lit(Lit::Int(val)) => self.code.push(Instruction::LoadConst(dst, *val)),
            Expr::Ident(ident) => {
                let reg = self.get_var(ident, span, true)?.reg;
                self.code.push(Instruction::Move(dst, reg));
            }
            Expr::Op1(Op1, expr) => {
                self.generate_expr(expr, span, dst)?;
                self.code.push(Instruction::Neg(dst));
            }
            Expr::Op2(op2, lhs, rhs) => {
                self.generate_expr(lhs, span, dst)?;
                let rhs_reg = self.alloc_reg();
                self.generate_expr(rhs, span, rhs_reg)?;
                let f = match op2 {
                    Op2::Add => Instruction::Add,
                    Op2::Sub => Instruction::Sub,
                    Op2::Mul => Instruction::Mul,
                    Op2::Div => Instruction::Div,
                    Op2::Mod => Instruction::Mod,
                };
                self.code.push(f(dst, rhs_reg));
            }
        }
        Ok(())
    }

    fn get_var(
        &mut self,
        ident: &Ident<'a>,
        span: &SrcSpan,
        read: bool,
    ) -> Result<&mut Variable<'a>, SemanticError<'a>> {
        self.vars
            .iter_mut()
            .find(|var| &var.ident == ident)
            .ok_or(SemanticError::UndefinedVariable(*ident, *span))
            .and_then(|var| {
                if !read || var.assigned {
                    Ok(var)
                } else {
                    Err(SemanticError::UnassignedVariable {
                        ident: *ident,
                        declaration: var.span,
                        usage: *span,
                    })
                }
            })
    }

    fn alloc_var(
        &mut self,
        ident: Ident<'a>,
        span: SrcSpan,
        reg: Register,
        assigned: bool,
    ) -> Result<(), SemanticError<'a>> {
        if let Some(var) = self.vars.iter().find(|var| var.ident == ident) {
            return Err(SemanticError::VariableRedefinition {
                ident,
                definition1: var.span,
                definition2: span,
            });
        }
        self.vars.push(Variable {
            ident,
            span,
            reg,
            assigned,
        });
        Ok(())
    }

    fn alloc_reg(&mut self) -> Register {
        let reg = Register(self.next_reg_id);
        self.next_reg_id += 1;
        reg
    }

    pub fn code(&self) -> &[Instruction] {
        &self.code
    }
}

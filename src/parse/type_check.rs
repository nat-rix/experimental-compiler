use crate::{
    error::{AnaError, AsgnIncompatibleTypeError, Op2IncompatibleTypeError},
    parse::AsgnOp,
    span::{Span, Spanned},
};

use super::{
    Asgn, Ast, Block, Ctrl, CtrlFor, CtrlIf, CtrlReturn, CtrlWhile, Decl, Expr, ExprAtom, ExprOp1,
    Function, Simp, Stmt, Type, TypeName, tokenize::Ident,
};

pub trait TypeCheck<'a> {
    type Context;
    type Ret;
    fn type_check(&self, ctx: Self::Context) -> Result<Self::Ret, AnaError<'a>>;
}

impl<'a> TypeCheck<'a> for Ast<'a> {
    type Context = ();
    type Ret = ();
    fn type_check(&self, ctx: Self::Context) -> Result<(), AnaError<'a>> {
        self.main_fun.type_check(ctx)
    }
}

impl<'a> TypeCheck<'a> for Function<'a> {
    type Context = ();
    type Ret = ();
    fn type_check(&self, _ctx: Self::Context) -> Result<(), AnaError<'a>> {
        let ctx = self.block.type_check(BlockContext {
            return_type: self.return_ty,
            vars: Default::default(),
            nest: 0,
            in_loop: false,
            returns: false,
        })?;
        if !ctx.returns {
            return Err(AnaError::ReturnCheck(
                self.return_ty.span().combine(self.block.span),
            ));
        }
        Ok(())
    }
}

fn check_cond<'a>(
    ctx: BlockContext<'a>,
    cond: &Expr<'a>,
) -> Result<BlockContext<'a>, AnaError<'a>> {
    let (ctx, cond_ty) = cond.type_check(ctx)?;
    if cond_ty != Type::Bool {
        return Err(AnaError::ConditionTypeIncompatible {
            cond: cond.span(),
            ty: cond_ty,
        });
    }
    Ok(ctx)
}

#[derive(Debug, Clone)]
pub struct Var<'a> {
    ty: TypeName,
    name: Ident<'a>,
    assigned: bool,
    nest: usize,
    decl_span: Span,
}

#[derive(Debug, Clone, Default)]
pub struct Vars<'a> {
    vars: Vec<Var<'a>>,
}

impl<'a> Vars<'a> {
    pub fn drop_lower_than(&mut self, nest: usize) {
        self.vars.retain(|v| v.nest <= nest)
    }

    pub fn get<'b>(&self, ident: &Ident<'b>) -> Option<&Var<'a>> {
        self.vars.iter().find(|var| &var.name == ident)
    }

    pub fn get_mut<'b>(&mut self, ident: &Ident<'b>) -> Option<&mut Var<'a>> {
        self.vars.iter_mut().find(|var| &var.name == ident)
    }

    pub fn get_mut_err(
        &mut self,
        ident: &Spanned<Ident<'a>>,
    ) -> Result<&mut Var<'a>, AnaError<'a>> {
        self.get_mut(ident)
            .ok_or(AnaError::UndeclaredVariable { ident: *ident })
    }
}

#[derive(Debug, Clone)]
pub struct BlockContext<'a> {
    return_type: TypeName,
    vars: Vars<'a>,
    nest: usize,
    in_loop: bool,
    returns: bool,
}

impl<'a> BlockContext<'a> {
    pub fn incr(&mut self) {
        self.nest += 1;
    }

    pub fn decr(&mut self) {
        self.nest -= 1;
        self.vars.drop_lower_than(self.nest);
    }
}

impl<'a> TypeCheck<'a> for Block<'a> {
    type Context = BlockContext<'a>;
    type Ret = Self::Context;
    fn type_check(&self, mut ctx: Self::Context) -> Result<Self::Ret, AnaError<'a>> {
        for stmt in &self.stmts {
            ctx = stmt.type_check(ctx)?;
        }
        Ok(ctx)
    }
}

impl<'a> TypeCheck<'a> for Stmt<'a> {
    type Context = BlockContext<'a>;
    type Ret = Self::Context;
    fn type_check(&self, mut ctx: Self::Context) -> Result<Self::Ret, AnaError<'a>> {
        match self {
            Self::Simp(v) => v.simp.type_check(ctx),
            Self::Ctrl(v) => v.type_check(ctx),
            Self::Block(block) => {
                ctx.incr();
                ctx = block.type_check(ctx)?;
                ctx.decr();
                Ok(ctx)
            }
        }
    }
}

impl<'a> TypeCheck<'a> for Simp<'a> {
    type Context = BlockContext<'a>;
    type Ret = Self::Context;
    fn type_check(&self, ctx: Self::Context) -> Result<Self::Ret, AnaError<'a>> {
        match self {
            Self::Asgn(v) => v.type_check(ctx),
            Self::Decl(v) => v.type_check(ctx),
        }
    }
}

impl<'a> TypeCheck<'a> for Asgn<'a> {
    type Context = BlockContext<'a>;
    type Ret = Self::Context;
    fn type_check(&self, mut ctx: Self::Context) -> Result<Self::Ret, AnaError<'a>> {
        let var = ctx.vars.get_mut_err(&self.lvalue)?;
        if !matches!(&self.op, AsgnOp::Asgn(_)) && !var.assigned && !ctx.returns {
            return Err(AnaError::UnassignedVariable { ident: self.lvalue });
        }
        let (mut ctx, rhs_ty) = self.expr.type_check(ctx)?;
        let var = ctx.vars.get_mut_err(&self.lvalue)?;
        if !self.op.is_type_compatible(&var.ty.into(), &rhs_ty) {
            return Err(AsgnIncompatibleTypeError {
                op: self.op,
                lhs_span: self.lvalue.span,
                rhs_span: self.expr.span(),
                lhs_ty: var.ty,
                rhs_ty,
            }
            .into());
        }
        var.assigned = true;
        Ok(ctx)
    }
}

impl<'a> TypeCheck<'a> for Expr<'a> {
    type Context = BlockContext<'a>;
    type Ret = (Self::Context, Type);
    fn type_check(&self, ctx: Self::Context) -> Result<Self::Ret, AnaError<'a>> {
        match self {
            Self::Atom(atom) => atom.type_check(ctx),
            Self::Op2(lhs, op, rhs) => {
                let (ctx, lhs_ty) = lhs.type_check(ctx)?;
                let (ctx, rhs_ty) = rhs.type_check(ctx)?;
                let Some(ret_ty) = op.get_return_ty(&lhs_ty, &rhs_ty) else {
                    return Err(Op2IncompatibleTypeError {
                        op: *op,
                        lhs_span: lhs.span(),
                        rhs_span: rhs.span(),
                        lhs_ty,
                        rhs_ty,
                    }
                    .into());
                };
                Ok((ctx, ret_ty))
            }
            Self::Op3(cond, _, lhs, _, rhs) => {
                let ctx = check_cond(ctx, cond)?;
                let (ctx, lhs_ty) = lhs.type_check(ctx)?;
                let (ctx, rhs_ty) = rhs.type_check(ctx)?;
                if lhs_ty != rhs_ty {
                    return Err(AnaError::TernaryValuesIncompatible {
                        lhs_span: lhs.span(),
                        rhs_span: rhs.span(),
                        lhs_ty,
                        rhs_ty,
                    });
                }
                Ok((ctx, lhs_ty))
            }
        }
    }
}

impl<'a> TypeCheck<'a> for ExprAtom<'a> {
    type Context = BlockContext<'a>;
    type Ret = (Self::Context, Type);
    fn type_check(&self, mut ctx: Self::Context) -> Result<Self::Ret, AnaError<'a>> {
        match self {
            Self::BoolConst(_) => Ok((ctx, Type::Bool)),
            Self::IntConst(_) => Ok((ctx, Type::Int)),
            Self::Ident(ident) => {
                let var = ctx.vars.get_mut_err(ident)?;
                if !var.assigned && !ctx.returns {
                    return Err(AnaError::UnassignedVariable { ident: *ident });
                }
                let ty = var.ty;
                Ok((ctx, ty.into()))
            }
            Self::Op1(expr) => expr.type_check(ctx),
        }
    }
}

impl<'a> TypeCheck<'a> for ExprOp1<'a> {
    type Context = BlockContext<'a>;
    type Ret = (Self::Context, Type);
    fn type_check(&self, ctx: Self::Context) -> Result<Self::Ret, AnaError<'a>> {
        let (ctx, ty) = self.expr.type_check(ctx)?;
        if !self.op.is_type_compatible(&ty) {
            return Err(AnaError::Op1IncompatibleType {
                op: self.op,
                ty,
                span: self.expr.span(),
            });
        }
        Ok((ctx, ty))
    }
}

impl<'a> TypeCheck<'a> for Decl<'a> {
    type Context = BlockContext<'a>;
    type Ret = Self::Context;
    fn type_check(&self, mut ctx: Self::Context) -> Result<Self::Ret, AnaError<'a>> {
        if let Some(rhs) = &self.val {
            let rhs_ty;
            (ctx, rhs_ty) = rhs.expr.type_check(ctx)?;
            if self.ty.ty() != rhs_ty {
                return Err(AsgnIncompatibleTypeError {
                    op: AsgnOp::Asgn(rhs.eq),
                    lhs_span: self.name.span,
                    rhs_span: rhs.expr.span(),
                    lhs_ty: self.ty,
                    rhs_ty,
                }
                .into());
            }
        }
        if let Some(var) = ctx.vars.get_mut(&self.name) {
            return Err(AnaError::VarRedeclaration {
                orig: var.decl_span,
                decl: self.name.span,
                ident: self.name.val,
            });
        }
        ctx.vars.vars.push(Var {
            ty: self.ty,
            name: self.name.val,
            assigned: self.val.is_some(),
            nest: ctx.nest,
            decl_span: self.name.span,
        });
        Ok(ctx)
    }
}

impl<'a> TypeCheck<'a> for Ctrl<'a> {
    type Context = BlockContext<'a>;
    type Ret = Self::Context;
    fn type_check(&self, ctx: Self::Context) -> Result<Self::Ret, AnaError<'a>> {
        match self {
            Ctrl::If(v) => v.type_check(ctx),
            Ctrl::While(v) => v.type_check(ctx),
            Ctrl::For(v) => v.type_check(ctx),
            Ctrl::Continue(v) => {
                if !ctx.in_loop {
                    return Err(AnaError::CtrlOpOutsideLoop(v.keyword.span));
                }
                Ok(ctx)
            }
            Ctrl::Break(v) => {
                if !ctx.in_loop {
                    return Err(AnaError::CtrlOpOutsideLoop(v.keyword.span));
                }
                Ok(ctx)
            }
            Ctrl::Return(v) => v.type_check(ctx),
        }
    }
}

impl<'a> TypeCheck<'a> for CtrlIf<'a> {
    type Context = BlockContext<'a>;
    type Ret = Self::Context;
    fn type_check(&self, ctx: Self::Context) -> Result<Self::Ret, AnaError<'a>> {
        let mut pre_ctx = check_cond(ctx, &self.cond)?;
        let then_ctx = self.stmt.type_check(pre_ctx.clone())?;
        if let Some(s) = &self.else_block {
            let else_ctx = s.stmt.type_check(pre_ctx.clone())?;
            for var in &mut pre_ctx.vars.vars {
                if !var.assigned
                    && then_ctx.vars.get(&var.name).is_some_and(|v| v.assigned)
                    && else_ctx.vars.get(&var.name).is_some_and(|v| v.assigned)
                {
                    var.assigned = true;
                }
            }
            if then_ctx.returns && else_ctx.returns {
                pre_ctx.returns = true;
            }
        }
        Ok(pre_ctx)
    }
}

impl<'a> TypeCheck<'a> for CtrlWhile<'a> {
    type Context = BlockContext<'a>;
    type Ret = Self::Context;
    fn type_check(&self, ctx: Self::Context) -> Result<Self::Ret, AnaError<'a>> {
        let ctx = check_cond(ctx, &self.cond)?;
        let mut block_ctx = ctx.clone();
        block_ctx.in_loop = true;
        self.stmt.type_check(block_ctx)?;
        Ok(ctx)
    }
}

impl<'a> TypeCheck<'a> for CtrlFor<'a> {
    type Context = BlockContext<'a>;
    type Ret = Self::Context;
    fn type_check(&self, mut ctx: Self::Context) -> Result<Self::Ret, AnaError<'a>> {
        ctx.incr();

        if let Some(op) = &self.op0 {
            ctx = op.type_check(ctx)?;
        }
        ctx = check_cond(ctx, &self.op1)?;
        if let Some(op) = &self.op2 {
            op.type_check(ctx.clone())?;
        }

        let mut block_ctx = ctx.clone();
        block_ctx.in_loop = true;
        self.stmt.type_check(block_ctx)?;

        ctx.decr();
        Ok(ctx)
    }
}

impl<'a> TypeCheck<'a> for CtrlReturn<'a> {
    type Context = BlockContext<'a>;
    type Ret = Self::Context;
    fn type_check(&self, ctx: Self::Context) -> Result<Self::Ret, AnaError<'a>> {
        let (mut ctx, ty) = self.expr.type_check(ctx)?;
        if ty != ctx.return_type.ty() {
            return Err(AnaError::WrongReturnType {
                expected: ctx.return_type,
                got: ty,
                span: self.expr.span(),
            });
        }
        ctx.returns = true;
        Ok(ctx)
    }
}

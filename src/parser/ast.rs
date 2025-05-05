use super::span::Spanned;

#[derive(Debug, Clone)]
pub struct Ast<'a> {
    pub stmts: Vec<Spanned<'a, Stmt<'a>>>,
}

#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Decl(Spanned<'a, Ident<'a>>),
    Init(Spanned<'a, Ident<'a>>, Spanned<'a, Expr<'a>>),
    Asgn(
        Spanned<'a, Ident<'a>>,
        Spanned<'a, AsgnOp>,
        Spanned<'a, Expr<'a>>,
    ),
    Ret(Spanned<'a, Expr<'a>>),
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Lit(Lit),
    Ident(Ident<'a>),
    Op1(Spanned<'a, Op1>, Spanned<'a, Box<Expr<'a>>>),
    Op2(
        Spanned<'a, Op2>,
        Spanned<'a, Box<Expr<'a>>>,
        Spanned<'a, Box<Expr<'a>>>,
    ),
}

#[derive(Debug, Clone)]
pub enum Lit {
    Int(IntLit),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntLit(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident<'a>(pub &'a [u8]);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AsgnOp {
    Asg,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Op1;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op2 {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

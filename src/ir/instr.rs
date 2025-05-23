#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Reg(pub(super) usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Label(pub(super) usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Cond {
    NotZero(Reg),
    Zero(Reg),
    None,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instr {
    Op1(Reg, Reg, Op1),
    Op2(Reg, [Reg; 2], Op2),
    DivMod([Reg; 2], [Reg; 2]),
    Mov(Reg, Reg),
    Ld(Reg, i32),
    Jmp(Cond, Label),
    Ret(Reg),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op1 {
    BNot,
    LNot,
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op2 {
    Add,
    Sub,
    Mul,
    And,
    Xor,
    Or,
    Shl,
    Shr,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
}

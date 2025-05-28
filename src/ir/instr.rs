#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Reg(pub(super) usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Label(pub(super) usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instr {
    Op1(Reg, Reg, Op1),
    Op2(Reg, [Reg; 2], Op2),
    DivMod([Reg; 2], [Reg; 2]),
    Mov(Reg, Reg),
    Ld(Reg, i32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Labels {
    pub non_zero: Label,
    pub zero: Label,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlockTail {
    JmpNoCond(Label),
    JmpCond(Reg, Labels),
    Ret(Reg),
}

impl BlockTail {
    pub fn jmp_targets(&self) -> impl Iterator<Item = Label> {
        (match self {
            Self::JmpCond(_, Labels { non_zero, zero }) => [Some(non_zero), Some(zero)],
            Self::JmpNoCond(label) => [Some(label), None],
            _ => [None; 2],
        })
        .into_iter()
        .filter_map(|label| label.copied())
    }

    pub const fn is_final(&self) -> bool {
        matches!(self, Self::Ret(_))
    }
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

macro_rules! impl_regs {
    ([$f:ident $($m:ident)?]: $($p:pat => ($d:expr, $s:expr)),*$(,)? ) => {
        #[allow(dead_code)]
        pub const fn $f ( &$($m)? self ) -> (&$($m)? [Reg], &$($m)? [Reg]) {
            struct Wrapped<T>(T);
            impl<'b, const N: usize> Wrapped<&'b $($m)? [Reg; N]> { pub const fn f(self) -> &'b $($m)? [Reg] { self.0 } }
            impl<'b> Wrapped<&'b Reg> { pub const fn f(self) -> &'b [Reg] { core::slice::from_ref(self.0) } }
            impl<'b> Wrapped<&'b mut Reg> { pub const fn f(self) -> &'b mut [Reg] { core::slice::from_mut(self.0) } }
            impl<'b> Wrapped<()> { pub const fn f(self) -> &'b $($m)? [Reg] { & $($m)? [] } }
            match self {
                $($p => ( Wrapped($d).f() , Wrapped($s).f() )),*
            }
        }
    };
    ($f_ref:ident $f_mut:ident: $($p:pat => ($d:expr, $s:expr)),*$(,)? ) => {
        impl_regs!([$f_ref]: $($p => ($d, $s)),*);
        impl_regs!([$f_mut mut]: $($p => ($d, $s)),*);
    };
    ($(,)?) => {};
}

impl Instr {
    impl_regs! { split_regs split_regs_mut:
        Self::Op1(d, s, _) | Self::Mov(d, s) => (d, s),
        Self::Op2(d, s, _) => (d, s),
        Self::DivMod(d, s) => (d, s),
        Self::Ld(d, _) => (d, ()),
    }
}

impl BlockTail {
    impl_regs! { split_regs split_regs_mut:
        Self::JmpNoCond(..) => ((), ()),
        Self::JmpCond(s, ..) | Self::Ret(s) => ((), s),
    }
}

use super::regs::{Reg, RegOrStack, StackOff};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Rex(u8);

impl Rex {
    pub const NONE: Self = Self(0);
    pub const REXW: Self = Self(0x48);
    pub const REXR: Self = Self(0x44);
    pub const REXX: Self = Self(0x42);
    pub const REXB: Self = Self(0x41);
    pub const REX: Self = Self(0x40);

    pub const fn code(&self) -> Option<core::num::NonZeroU8> {
        core::num::NonZeroU8::new(self.0)
    }

    pub const fn combine(self, rhs: Self) -> Self {
        Self(self.0 | rhs.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mode {
    Mod0,
    Mod1,
    Mod2,
    Mod3,
}

impl Mode {
    pub const fn index(&self) -> u8 {
        match self {
            Self::Mod0 => 0,
            Self::Mod1 => 1,
            Self::Mod2 => 2,
            Self::Mod3 => 3,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SibMul {
    Mul1,
    Mul2,
    Mul4,
    Mul8,
}

impl SibMul {
    pub const fn index(&self) -> u8 {
        match self {
            Self::Mul1 => 0,
            Self::Mul2 => 1,
            Self::Mul4 => 2,
            Self::Mul8 => 3,
        }
    }

    pub const fn try_from_i32(val: i32) -> Option<Self> {
        Some(match val {
            1 => Self::Mul1,
            2 => Self::Mul2,
            4 => Self::Mul4,
            8 => Self::Mul8,
            _ => return None,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Sib {
    pub mul: SibMul,
    index: Reg,
    base: Reg,
}

impl Sib {
    pub const NONE: Self = Self {
        mul: SibMul::Mul1,
        index: Reg::EAX,
        base: Reg::EAX,
    };

    pub const fn code(&self) -> u8 {
        (self.mul.index() << 6) | ((self.index.index() & 7) << 3) | (self.base.index() & 7)
    }
}

pub struct ModRm {
    mode: Mode,
    reg: Reg,
    rm: Reg,
    sib: Sib,
    disp: i32,
    is_bytereg: bool,
}

impl ModRm {
    pub fn from_rmreg(reg: Reg) -> Self {
        Self {
            mode: Mode::Mod3,
            rm: reg,
            reg: Reg::EAX,
            sib: Sib::NONE,
            disp: 0,
            is_bytereg: false,
        }
    }

    /// Returns `None` if any of these conditions are met:
    /// - `index` is esp or r12 and `mul` != 1
    /// - `index` and `base` are both esp or r12
    pub fn from_sib(mut base: Reg, mut index: Option<Reg>, mul: SibMul, disp: i32) -> Option<Self> {
        if let Some(index @ (Reg::ESP | Reg::R12)) = &mut index {
            if !matches!(&mul, SibMul::Mul1) {
                // esp cannot be multiplied in sib
                return None;
            }
            if matches!(base, Reg::ESP | Reg::R12) {
                // base and index cannot both be esp
                return None;
            }
            core::mem::swap(index, &mut base);
        }
        // index register now cannot be esp
        let mut force_disp8 = false;
        if disp == 0 && matches!(base, Reg::EBP | Reg::R13) {
            match (mul, &mut index) {
                (_, None | Some(Reg::EBP | Reg::R13))
                | (SibMul::Mul2 | SibMul::Mul4 | SibMul::Mul8, _) => {
                    force_disp8 = true;
                }
                (SibMul::Mul1, Some(index)) => {
                    // instead of forcing disp8, do [ebp * 1 + index]
                    core::mem::swap(index, &mut base);
                }
            }
        }
        let mode = match (force_disp8, disp) {
            (true, _) => Mode::Mod1,
            (_, 0) => Mode::Mod0,
            (_, -0x80..=0x7f) => Mode::Mod1,
            _ => Mode::Mod2,
        };
        Some(Self {
            mode,
            rm: Reg::ESP.with_msb_of(base),
            reg: Reg::EAX,
            sib: Sib {
                mul,
                index: index.unwrap_or(Reg::ESP),
                base,
            },
            disp,
            is_bytereg: false,
        })
    }

    pub fn with_reg(mut self, reg: Reg) -> Self {
        self.reg = reg;
        self
    }

    pub fn with_opext(self, reg: u8) -> Self {
        self.with_reg(reg.into())
    }

    pub fn with_byereg(mut self) -> Self {
        self.is_bytereg = true;
        self
    }

    pub fn rm_rex(&self) -> Rex {
        if self.rm.msb() { Rex::REXB } else { Rex::NONE }
    }

    pub fn reg_rex(&self) -> Rex {
        if self.reg.msb() { Rex::REXR } else { Rex::NONE }
    }

    pub fn sib_rex(&self) -> Rex {
        if self.sib.index.msb() {
            Rex::REXX
        } else {
            Rex::NONE
        }
    }

    pub fn bytereg_rex(&self) -> Rex {
        if self.is_bytereg
            && (Reg::ESP..=Reg::EDI).contains(&self.rm)
            && matches!(self.mode, Mode::Mod3)
        {
            Rex::REX
        } else {
            Rex::NONE
        }
    }

    pub fn rex(&self) -> Rex {
        self.rm_rex()
            .combine(self.reg_rex())
            .combine(self.sib_rex())
            .combine(self.bytereg_rex())
    }

    pub const fn has_sib(&self) -> bool {
        matches!(
            (self.mode, self.rm),
            (Mode::Mod0 | Mode::Mod1 | Mode::Mod2, Reg::ESP | Reg::R12)
        )
    }

    pub const fn has_disp8(&self) -> bool {
        matches!(&self.mode, Mode::Mod1)
    }

    pub const fn has_disp32(&self) -> bool {
        matches!(
            (self.mode, self.rm),
            (Mode::Mod0, Reg::EBP | Reg::R13) | (Mode::Mod2, _)
        )
    }

    pub fn encode(&self, buffer: &mut Vec<u8>) {
        buffer
            .push((self.mode.index() << 6) | ((self.reg.index() & 7) << 3) | (self.rm.index() & 7));
        if self.has_sib() {
            buffer.push(self.sib.code());
        }
        if self.has_disp8() {
            buffer.push(self.disp as _);
        } else if self.has_disp32() {
            buffer.extend_from_slice(&self.disp.to_le_bytes());
        }
    }
}

impl From<Reg> for ModRm {
    fn from(value: Reg) -> Self {
        Self::from_rmreg(value)
    }
}

impl From<StackOff> for ModRm {
    fn from(value: StackOff) -> Self {
        Self::from_sib(Reg::ESP, None, SibMul::Mul1, value.0).unwrap()
    }
}

impl From<RegOrStack> for ModRm {
    fn from(value: RegOrStack) -> Self {
        match value {
            RegOrStack::Reg(v) => v.into(),
            RegOrStack::Stack(v) => v.into(),
        }
    }
}

pub struct InstrEnc<const N: usize> {
    rex: Rex,
    opcode: [u8; N],
    modrm: Option<ModRm>,
}

impl<const N: usize> InstrEnc<N> {
    pub const fn new(opcode: [u8; N]) -> Self {
        Self {
            rex: Rex::NONE,
            opcode,
            modrm: None,
        }
    }

    pub fn with_modrm(mut self, modrm: ModRm) -> Self {
        self.rex = self.rex.combine(modrm.rex());
        self.modrm = Some(modrm);
        self
    }

    pub fn with_intreg(mut self, reg: Reg) -> Self {
        *self.opcode.last_mut().unwrap() |= reg.index() & 7;
        if reg.msb() {
            self.rex = self.rex.combine(Rex::REXB);
        }
        self
    }

    pub fn with_64bit(mut self) -> Self {
        self.rex = self.rex.combine(Rex::REXW);
        self
    }

    pub fn encode(&self, buffer: &mut Vec<u8>) {
        if self.rex != Rex::NONE {
            buffer.push(self.rex.0);
        }
        buffer.extend_from_slice(&self.opcode);
        if let Some(modrm) = &self.modrm {
            modrm.encode(buffer);
        }
    }
}

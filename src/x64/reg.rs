#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Rex(u8);

impl Rex {
    pub const NONE: Self = Self(0);
    pub const REXW: Self = Self(0x48);
    pub const REXR: Self = Self(0x44);
    pub const REXX: Self = Self(0x42);
    pub const REXB: Self = Self(0x41);

    pub const fn code(&self) -> Option<core::num::NonZeroU8> {
        core::num::NonZeroU8::new(self.0)
    }
}

impl core::ops::Add for Rex {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        #[allow(clippy::suspicious_arithmetic_impl)]
        Self(self.0 | rhs.0)
    }
}

impl core::ops::AddAssign for Rex {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RegNoExt {
    /// eax / r8
    R0,
    /// ecx / r9
    R1,
    /// edx / r10
    R2,
    /// ebx / r11
    R3,
    /// esp / r12
    R4,
    /// ebp / r13
    R5,
    /// esi / r14
    R6,
    /// edi / r15
    R7,
}

impl RegNoExt {
    pub const fn index(&self) -> u8 {
        match self {
            Self::R0 => 0,
            Self::R1 => 1,
            Self::R2 => 2,
            Self::R3 => 3,
            Self::R4 => 4,
            Self::R5 => 5,
            Self::R6 => 6,
            Self::R7 => 7,
        }
    }
}

pub trait Ext: Eq {
    fn is_high(&self) -> bool;
    fn offset(&self) -> u8 {
        if self.is_high() { 8 } else { 0 }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtLo;
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtHi;
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtAny(bool);
impl Ext for ExtLo {
    fn is_high(&self) -> bool {
        false
    }
}
impl Ext for ExtHi {
    fn is_high(&self) -> bool {
        true
    }
}
impl Ext for ExtAny {
    fn is_high(&self) -> bool {
        self.0
    }
}

impl ExtAny {
    pub const LO: Self = Self(false);
    pub const HI: Self = Self(false);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Reg<E>(pub RegNoExt, pub E);

impl<E: Ext> Reg<E> {
    pub const fn index_raw(&self) -> u8 {
        self.0.index()
    }
    pub fn index_full(&self) -> u8 {
        self.0.index() + self.1.offset()
    }
    pub fn rexr(&self) -> Rex {
        if self.1.is_high() {
            Rex::REXR
        } else {
            Rex::NONE
        }
    }
    pub fn rexb(&self) -> Rex {
        if self.1.is_high() {
            Rex::REXB
        } else {
            Rex::NONE
        }
    }
}

impl RegNoExt {
    pub const fn from_index(index: u8) -> Self {
        match index & 7 {
            0 => Self::R0,
            1 => Self::R1,
            2 => Self::R2,
            3 => Self::R3,
            4 => Self::R4,
            5 => Self::R5,
            6 => Self::R6,
            _ => Self::R7,
        }
    }
}

impl Reg<ExtAny> {
    pub const EAX: Self = Self::from_index(0);
    pub const ECX: Self = Self::from_index(1);
    pub const EDX: Self = Self::from_index(2);
    pub const EBX: Self = Self::from_index(3);
    pub const ESP: Self = Self::from_index(4);
    pub const EBP: Self = Self::from_index(5);
    pub const ESI: Self = Self::from_index(6);
    pub const EDI: Self = Self::from_index(7);
    pub const R8: Self = Self::from_index(8);
    pub const R9: Self = Self::from_index(9);
    pub const R10: Self = Self::from_index(10);
    pub const R11: Self = Self::from_index(11);
    pub const R12: Self = Self::from_index(12);
    pub const R13: Self = Self::from_index(13);
    pub const R14: Self = Self::from_index(14);
    pub const R15: Self = Self::from_index(15);

    pub const fn from_index(index: u8) -> Self {
        Self(RegNoExt::from_index(index), ExtAny(index >= 8))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum RmMod {
    Mod0,
    Mod1,
    Mod2,
    Mod3,
}

impl RmMod {
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
}

#[derive(Debug, Clone, Copy)]
pub struct Sib {
    pub mul: SibMul,
    index: Reg<ExtAny>,
    base: RegNoExt,
}

impl Sib {
    pub const NONE: Self = Self {
        mul: SibMul::Mul1,
        index: Reg::EAX,
        base: RegNoExt::R0,
    };

    pub const fn code(&self) -> u8 {
        (self.mul.index() << 6) | (self.index.index_raw() << 3) | self.base.index()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Rm32 {
    rmmod: RmMod,
    rm: Reg<ExtAny>,
    sib: Sib,
    disp: i32,
}

impl Rm32 {
    pub fn from_rmreg(reg: Reg<ExtAny>) -> Self {
        Self {
            rmmod: RmMod::Mod3,
            rm: reg,
            sib: Sib::NONE,
            disp: 0,
        }
    }

    /// Returns `None` if any of these conditions are met:
    /// - `index` is esp or r12 and `mul` != 1
    /// - `index` and `base` are both esp or r12
    pub fn from_sib(
        mut base: Reg<ExtAny>,
        mut index: Option<Reg<ExtAny>>,
        mul: SibMul,
        disp: i32,
    ) -> Option<Self> {
        if let Some(index @ Reg(RegNoExt::R4, _)) = &mut index {
            if !matches!(&mul, SibMul::Mul1) {
                // esp cannot be multiplied in sib
                return None;
            }
            if matches!(&base, Reg(RegNoExt::R4, _)) {
                // base and index cannot both be esp
                return None;
            }
            core::mem::swap(index, &mut base);
        }
        // index register now cannot be esp
        let mut force_disp8 = false;
        if disp == 0 && matches!(&base, Reg(RegNoExt::R5, _)) {
            match (mul, &mut index) {
                (_, None | Some(Reg(RegNoExt::R5, _)))
                | (SibMul::Mul2 | SibMul::Mul4 | SibMul::Mul8, _) => {
                    force_disp8 = true;
                }
                (SibMul::Mul1, Some(index)) => {
                    // instead of forcing disp8, do [ebp * 1 + index]
                    core::mem::swap(index, &mut base);
                }
            }
        }
        let rmmod = match (force_disp8, disp) {
            (true, _) => RmMod::Mod1,
            (_, 0) => RmMod::Mod0,
            (_, -0x80..=0x7f) => RmMod::Mod1,
            _ => RmMod::Mod2,
        };
        Some(Self {
            rmmod,
            rm: Reg(RegNoExt::R4, base.1),
            sib: Sib {
                mul,
                index: index.unwrap_or(Reg::ESP),
                base: base.0,
            },
            disp,
        })
    }

    pub fn from_rsp_relative(off: i32) -> Self {
        Self::from_sib(Reg::ESP, None, SibMul::Mul1, off).unwrap()
    }

    pub fn try_into_reg(self) -> Option<Reg<ExtAny>> {
        matches!(self.rmmod, RmMod::Mod3).then_some(self.rm)
    }

    pub const fn has_disp8(&self) -> bool {
        matches!(&self.rmmod, RmMod::Mod1)
    }

    pub const fn has_disp32(&self) -> bool {
        matches!(
            (&self.rmmod, &self.rm.0),
            (RmMod::Mod0, RegNoExt::R5) | (RmMod::Mod2, _)
        )
    }

    pub const fn has_sib(&self) -> bool {
        matches!(
            (self.rmmod, &self.rm.0),
            (RmMod::Mod0 | RmMod::Mod1 | RmMod::Mod2, RegNoExt::R4)
        )
    }

    pub fn encode(&self, buffer: &mut Vec<u8>, regix: u8) {
        buffer.push((self.rmmod.index() << 6) | (regix << 3) | self.rm.index_raw());
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

impl Rm32 {
    pub fn rm_rex(&self) -> Rex {
        if self.rm.1.is_high() {
            Rex::REXB
        } else {
            Rex::NONE
        }
    }

    pub fn sib_rex(&self) -> Rex {
        if self.sib.index.1.is_high() {
            Rex::REXX
        } else {
            Rex::NONE
        }
    }

    pub fn rex(&self) -> Rex {
        self.rm_rex() + self.sib_rex()
    }
}

impl From<Reg<ExtAny>> for Rm32 {
    fn from(value: Reg<ExtAny>) -> Self {
        Rm32::from_rmreg(value)
    }
}

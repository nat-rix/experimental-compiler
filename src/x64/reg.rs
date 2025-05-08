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
    /// eax
    R0,
    /// ecx
    R1,
    /// edx
    R2,
    /// ebx
    R3,
    /// esp
    R4,
    /// ebp
    R5,
    /// esi
    R6,
    /// edi
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

pub trait Ext {
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

#[derive(Debug, Clone, Copy)]
pub struct Rm32<E> {
    rmmod: RmMod,
    rm: RegNoExt,
    disp: u32,
    pub ext: E,
}

impl<E> Rm32<E> {
    pub fn from_reg(reg: Reg<E>) -> Self {
        Self {
            rmmod: RmMod::Mod3,
            rm: reg.0,
            disp: 0,
            ext: reg.1,
        }
    }

    pub fn try_into_reg(self) -> Option<Reg<E>> {
        matches!(self.rmmod, RmMod::Mod3).then_some(Reg(self.rm, self.ext))
    }

    pub const fn has_disp8(&self) -> bool {
        matches!(&self.rmmod, RmMod::Mod1)
    }

    pub const fn has_disp32(&self) -> bool {
        matches!(
            (&self.rmmod, &self.rm),
            (RmMod::Mod0, RegNoExt::R5) | (RmMod::Mod2, _)
        )
    }

    pub fn encode(&self, buffer: &mut Vec<u8>, regix: u8) {
        buffer.push((self.rmmod.index() << 6) | (regix << 3) | self.rm.index());
        if self.has_disp8() {
            buffer.push(self.disp as _);
        } else if self.has_disp32() {
            buffer.extend_from_slice(&self.disp.to_le_bytes());
        }
    }
}

impl<E: Ext> Rm32<E> {
    pub fn rex(&self) -> Rex {
        if self.ext.is_high() {
            Rex::REXB
        } else {
            Rex::NONE
        }
    }
}

impl<E> From<Reg<E>> for Rm32<E> {
    fn from(value: Reg<E>) -> Self {
        Rm32::from_reg(value)
    }
}

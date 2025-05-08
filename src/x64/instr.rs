const REXW: u8 = 0x48;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Reg {
    Eax,
    Ecx,
    Edx,
    Ebx,
    Esp,
    Ebp,
    Esi,
    Edi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Reg {
    pub const fn index(&self) -> u8 {
        match self {
            Self::Eax => 0,
            Self::Ecx => 1,
            Self::Edx => 2,
            Self::Ebx => 3,
            Self::Esp => 4,
            Self::Ebp => 5,
            Self::Esi => 6,
            Self::Edi => 7,
            Self::R8 => 8,
            Self::R9 => 9,
            Self::R10 => 10,
            Self::R11 => 11,
            Self::R12 => 12,
            Self::R13 => 13,
            Self::R14 => 14,
            Self::R15 => 15,
        }
    }

    pub const fn from_index(index: u8) -> Self {
        match index & 15 {
            0 => Self::Eax,
            1 => Self::Ecx,
            2 => Self::Edx,
            3 => Self::Ebx,
            4 => Self::Esp,
            5 => Self::Ebp,
            6 => Self::Esi,
            7 => Self::Edi,
            8 => Self::R8,
            9 => Self::R9,
            10 => Self::R10,
            11 => Self::R11,
            12 => Self::R12,
            13 => Self::R13,
            14 => Self::R14,
            _ => Self::R15,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instr {
    Add(Reg, Reg),
    MovConst(Reg, u32),

    Mov64Const32SignExtend(Reg, i32),
    Syscall,
}

impl Instr {
    pub fn encode(&self, buffer: &mut Vec<u8>) {
        match self {
            Self::Add(dst, src) => {
                buffer.extend_from_slice(&[0x01, 0xc0 | (src.index() << 3) | dst.index()]);
            }
            Self::MovConst(dst, val) => {
                buffer.push(0xb8 | dst.index());
                buffer.extend_from_slice(&val.to_le_bytes());
            }
            Self::Mov64Const32SignExtend(dst, val) => {
                buffer.extend_from_slice(&[REXW, 0xc7, 0xc0 | dst.index()]);
                buffer.extend_from_slice(&val.to_le_bytes());
            }
            Self::Syscall => buffer.extend_from_slice(&[0x0f, 0x05]),
        }
    }
}

use crate::x64::reg::RegNoExt;

use super::reg::{ExtAny, Reg, Rex, Rm32};

#[derive(Debug, Clone)]
pub enum Instr {
    Add32RmReg(Rm32, Reg<ExtAny>),
    Add32RegRm(Reg<ExtAny>, Rm32),
    Add64RmImm32(Reg<ExtAny>, i32),
    Sub64RmImm32(Reg<ExtAny>, i32),
    Mov32RmImm(Rm32, u32),
    Lea32(Reg<ExtAny>, Rm32),

    Add8AlImm(u8),
    Xor64RmReg(Rm32, Reg<ExtAny>),
    Syscall,
}

fn encode_rm_or_reg<const N: usize>(
    buffer: &mut Vec<u8>,
    mut rex: Rex,
    mut opcode: [u8; N],
    rm: Option<&Rm32>,
    reg: Reg<ExtAny>,
) {
    rex += if rm.is_some() { reg.rexr() } else { reg.rexb() };
    if let Some(rm) = &rm {
        rex += rm.rex();
    } else if let Some(op) = opcode.last_mut() {
        *op |= reg.index_raw();
    }
    if let Some(code) = rex.code() {
        buffer.push(code.get());
    }
    buffer.extend_from_slice(&opcode);
    if let Some(rm) = rm {
        rm.encode(buffer, reg.index_raw());
    }
}

fn encode_rm<const N: usize>(buffer: &mut Vec<u8>, opcode: [u8; N], rm: &Rm32, reg: Reg<ExtAny>) {
    encode_rm_or_reg(buffer, Rex::NONE, opcode, Some(rm), reg);
}

fn encode_reg<const N: usize>(buffer: &mut Vec<u8>, opcode: [u8; N], reg: Reg<ExtAny>) {
    encode_rm_or_reg(buffer, Rex::NONE, opcode, None, reg);
}

const OP0: Reg<ExtAny> = Reg(RegNoExt::R0, ExtAny::LO);

impl Instr {
    pub fn encode(&self, buffer: &mut Vec<u8>) {
        match self {
            Self::Add32RmReg(rm, reg) => {
                encode_rm(buffer, [0x01], rm, *reg);
            }
            Self::Add32RegRm(reg, rm) => {
                encode_rm(buffer, [0x03], rm, *reg);
            }
            Self::Add64RmImm32(rm, imm) => {
                if *imm == 0 {
                    // this is like nop
                } else if let Ok(imm) = i8::try_from(*imm) {
                    encode_rm_or_reg(buffer, Rex::REXW, [0x83], Some(&(*rm).into()), Reg::EAX);
                    buffer.extend_from_slice(&imm.to_le_bytes());
                } else {
                    encode_rm_or_reg(buffer, Rex::REXW, [0x81], Some(&(*rm).into()), Reg::EAX);
                    buffer.extend_from_slice(&imm.to_le_bytes());
                }
            }
            Self::Sub64RmImm32(rm, imm) => {
                if *imm == 0 {
                    // this is like nop
                } else if let Ok(imm) = i8::try_from(*imm) {
                    encode_rm_or_reg(buffer, Rex::REXW, [0x83], Some(&(*rm).into()), Reg::EBP);
                    buffer.extend_from_slice(&imm.to_le_bytes());
                } else {
                    encode_rm_or_reg(buffer, Rex::REXW, [0x81], Some(&(*rm).into()), Reg::EBP);
                    buffer.extend_from_slice(&imm.to_le_bytes());
                }
            }
            Self::Mov32RmImm(rm, imm) => {
                if let Some(reg) = rm.try_into_reg() {
                    encode_reg(buffer, [0xb8], reg);
                } else {
                    encode_rm(buffer, [0xc7], rm, OP0);
                }
                buffer.extend_from_slice(&imm.to_le_bytes());
            }
            Self::Lea32(reg, rm) => {
                encode_rm(buffer, [0x8d], rm, *reg);
            }
            Self::Add8AlImm(val) => {
                buffer.extend_from_slice(&[0x04, *val]);
            }
            Self::Xor64RmReg(rm, reg) => {
                encode_rm_or_reg(buffer, Rex::REXW, [0x31], Some(rm), *reg);
            }
            Self::Syscall => buffer.extend_from_slice(&[0x0f, 0x05]),
        }
    }
}

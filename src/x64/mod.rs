pub mod instr;

use instr::{Instr, Reg};

use crate::aasm::{AReg, instr::Instr as AInstr, ssa::SsaBlock};

#[derive(Debug, Clone, Copy)]
pub struct StackOffset {
    off: usize,
}

#[derive(Debug, Clone)]
pub enum RegOrStack {
    Reg(Reg),
    Stack(StackOffset),
}

impl From<Reg> for RegOrStack {
    fn from(value: Reg) -> Self {
        Self::Reg(value)
    }
}

impl From<StackOffset> for RegOrStack {
    fn from(value: StackOffset) -> Self {
        Self::Stack(value)
    }
}

#[derive(Debug, Clone)]
pub struct RegMapping {
    map: Vec<Option<RegOrStack>>,
    used_regs: u16,
}

impl RegMapping {
    pub fn new(num: usize) -> Self {
        Self {
            map: vec![None; num],
            used_regs: 0,
        }
    }

    pub fn map(&mut self, areg: &AReg, val: impl Into<RegOrStack>) {
        let val: RegOrStack = val.into();
        if let RegOrStack::Reg(reg) = &val {
            self.used_regs |= 1 << reg.index();
        }
        self.map[areg.0] = Some(val)
    }

    pub fn alloc(&mut self) -> RegOrStack {
        if self.used_regs == u16::MAX {
            todo!()
        }
        Reg::from_index(self.used_regs.trailing_ones() as _).into()
    }

    pub fn get_or_insert<'a>(&'a mut self, areg: &AReg) -> &'a RegOrStack {
        if self.map[areg.0].is_some() {
            return self.map[areg.0].as_ref().unwrap();
        }
        let val = self.alloc();
        self.map[areg.0].insert(val)
    }
}

#[derive(Debug, Clone, Default)]
pub struct CodeGen {
    code: Vec<Instr>,
}

impl CodeGen {
    pub fn generate(&mut self, block: &SsaBlock<AReg>) {
        let mut mapping = RegMapping::new(block.reg_count());

        if let Some(reg) = block.return_reg() {
            mapping.map(reg, Reg::Edi);
        }

        for instr in block.code() {
            match instr {
                AInstr::LoadConst(areg, val) => {
                    let reg = mapping.get_or_insert(areg);
                    if let RegOrStack::Reg(reg) = reg {
                        self.code.push(Instr::MovConst(*reg, val.0));
                    } else {
                        todo!()
                    }
                }
                AInstr::Move(_, _) => todo!(),
                AInstr::Neg(_, _) => todo!(),
                AInstr::Add(d, [d2, s] | [s, d2]) if d == d2 => {
                    let s = mapping.get_or_insert(s).clone();
                    let d = mapping.get_or_insert(d).clone();
                    match (d, s) {
                        (RegOrStack::Reg(d), RegOrStack::Reg(s)) => {
                            self.code.push(Instr::Add(d, s));
                        }
                        _ => todo!(),
                    }
                }
                AInstr::Add(_, _) => todo!(),
                AInstr::Sub(_, _) => todo!(),
                AInstr::Mul(_, _) => todo!(),
                AInstr::Div(_, _) => todo!(),
                AInstr::Mod(_, _) => todo!(),
                AInstr::Return(_) => {
                    self.code
                        .push(Instr::Mov64Const32SignExtend(Reg::Eax, 0x3c));
                    self.code.push(Instr::Syscall);
                }
            }
        }
    }

    pub fn encode(&self, buffer: &mut Vec<u8>) {
        for instr in &self.code {
            instr.encode(buffer);
        }
    }
}

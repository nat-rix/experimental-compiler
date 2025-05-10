pub mod instr;
pub mod reg;

use instr::Instr;
use reg::{ExtAny, Reg, RegNoExt, Rm32};

use crate::{
    aasm::{AReg, instr::Instr as AInstr, ssa::SsaBlock},
    error::CodeGenError,
};

#[derive(Debug, Clone, Copy)]
pub struct StackOffset {
    off: i64,
}

impl StackOffset {
    pub fn difference(self, rhs: Self) -> Result<Option<i32>, CodeGenError> {
        self.off
            .checked_sub(rhs.off)
            .map(|off| i32::try_from(off).map_err(|_| CodeGenError::StackOffsetOverflow))
            .transpose()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RegOrStack {
    Reg(Reg<ExtAny>),
    Stack(StackOffset),
}

impl From<Reg<ExtAny>> for RegOrStack {
    fn from(value: Reg<ExtAny>) -> Self {
        Self::Reg(value)
    }
}

impl From<StackOffset> for RegOrStack {
    fn from(value: StackOffset) -> Self {
        Self::Stack(value)
    }
}

impl TryFrom<StackOffset> for Rm32 {
    type Error = CodeGenError;
    fn try_from(value: StackOffset) -> Result<Self, Self::Error> {
        if let Ok(off) = i32::try_from(value.off) {
            Ok(Rm32::from_rsp_relative(off))
        } else {
            Err(CodeGenError::StackOffsetOverflow)
        }
    }
}

impl TryFrom<RegOrStack> for Rm32 {
    type Error = CodeGenError;
    fn try_from(value: RegOrStack) -> Result<Self, Self::Error> {
        match value {
            RegOrStack::Reg(reg) => Ok(reg.into()),
            RegOrStack::Stack(off) => off.try_into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct RegMapping {
    map: Vec<Option<RegOrStack>>,
    used_regs: u16,
    stack_cursor: StackOffset,
}

impl RegMapping {
    pub fn new(num: usize) -> Self {
        Self {
            map: vec![None; num],
            used_regs: 1 << Reg::ESP.index_full(),
            stack_cursor: StackOffset { off: 0 },
        }
    }

    pub fn map(&mut self, areg: &AReg, val: impl Into<RegOrStack>) -> &RegOrStack {
        let val: RegOrStack = val.into();
        match &val {
            RegOrStack::Reg(reg) => {
                self.used_regs |= 1 << reg.index_full();
            }
            RegOrStack::Stack(stack) => {
                self.stack_cursor.off = stack.off + 4;
            }
        }
        self.map[areg.0].insert(val)
    }

    pub fn alloc(&mut self) -> RegOrStack {
        if self.used_regs == u16::MAX {
            RegOrStack::Stack(self.stack_cursor)
        } else {
            Reg::from_index(self.used_regs.trailing_ones() as _).into()
        }
    }

    pub fn get_or_insert<'a>(&'a mut self, areg: &AReg) -> &'a RegOrStack {
        if self.map[areg.0].is_some() {
            return self.map[areg.0].as_ref().unwrap();
        }
        let val = self.alloc();
        self.map(areg, val)
    }
}

#[derive(Debug, Clone, Default)]
pub struct CodeGen {
    code: Vec<Instr>,
}

fn combine<'d, 's, T: Eq>(d: &'d T, s1: &'s T, s2: &'s T) -> Option<(&'d T, &'s T)> {
    if d == s1 {
        Some((d, s2))
    } else if d == s2 {
        Some((d, s1))
    } else {
        None
    }
}

fn free_reg2(a: &RegOrStack, b: &RegOrStack) -> Reg<ExtAny> {
    match (a, b) {
        (RegOrStack::Reg(Reg::EAX), RegOrStack::Reg(Reg::EDX))
        | (RegOrStack::Reg(Reg::EDX), RegOrStack::Reg(Reg::EAX)) => Reg::EBX,
        (RegOrStack::Reg(Reg::EAX), _) | (_, RegOrStack::Reg(Reg::EAX)) => Reg::EDX,
        _ => Reg::EAX,
    }
}

fn free_reg2_or_xhcg(d: RegOrStack, s1: &RegOrStack, s2: &RegOrStack) -> Reg<ExtAny> {
    if let RegOrStack::Reg(d) = d {
        d
    } else {
        free_reg2(s1, s2)
    }
}

impl CodeGen {
    pub fn generate(&mut self, block: &SsaBlock<AReg>) -> Result<(), CodeGenError> {
        let mut mapping = RegMapping::new(block.reg_count());

        if let Some(reg) = block.return_reg() {
            mapping.map(reg, Reg(RegNoExt::R7, ExtAny::LO));
        }

        let start_stack = mapping.stack_cursor;

        for instr in block.code() {
            match instr {
                AInstr::LoadConst(areg, val) => {
                    let reg = mapping.get_or_insert(areg);
                    self.code.push(Instr::Mov32RmImm((*reg).try_into()?, val.0));
                }
                AInstr::Move(_, _) => todo!(),
                AInstr::Neg(_, _) => todo!(),
                AInstr::Add(d, [s1, s2]) => {
                    if let Some((d, s)) = combine(d, s1, s2) {
                        let d = *mapping.get_or_insert(d);
                        let s = *mapping.get_or_insert(s);
                        match (d, s) {
                            (d, RegOrStack::Reg(s)) => {
                                self.code.push(Instr::Add32RmReg(d.try_into()?, s));
                            }
                            (RegOrStack::Reg(d), s) => {
                                self.code.push(Instr::Add32RegRm(d, s.try_into()?));
                            }
                            (RegOrStack::Stack(d), RegOrStack::Stack(s)) => {
                                self.code.push(Instr::Xchg32RmReg(s.try_into()?, Reg::EAX));
                                self.code.push(Instr::Add32RmReg(d.try_into()?, Reg::EAX));
                                self.code.push(Instr::Xchg32RmReg(s.try_into()?, Reg::EAX));
                            }
                        }
                    } else {
                        let d = *mapping.get_or_insert(d);
                        let s1 = *mapping.get_or_insert(s1);
                        let s2 = *mapping.get_or_insert(s2);
                        let dtmp = free_reg2_or_xhcg(d, &s1, &s2);
                        let s1tmp = free_reg2_or_xhcg(s1, &d, &s2);
                        let s2tmp = free_reg2_or_xhcg(s2, &d, &s1);
                        if let RegOrStack::Stack(d) = &d {
                            self.code.push(Instr::Xchg32RmReg((*d).try_into()?, dtmp));
                        }
                        if let RegOrStack::Stack(s1) = &s1 {
                            self.code.push(Instr::Xchg32RmReg((*s1).try_into()?, s1tmp));
                        }
                        if let RegOrStack::Stack(s2) = &s2 {
                            self.code.push(Instr::Xchg32RmReg((*s2).try_into()?, s2tmp));
                        }
                        self.code.push(Instr::Lea32(
                            dtmp,
                            Rm32::from_sib(s1tmp, Some(s2tmp), reg::SibMul::Mul1, 0)
                                .ok_or(CodeGenError::InvalidEspAccess)?,
                        ));
                        if let RegOrStack::Stack(d) = &d {
                            self.code.push(Instr::Xchg32RmReg((*d).try_into()?, dtmp));
                        }
                        if let RegOrStack::Stack(s1) = &s1 {
                            self.code.push(Instr::Xchg32RmReg((*s1).try_into()?, s1tmp));
                        }
                        if let RegOrStack::Stack(s2) = &s2 {
                            self.code.push(Instr::Xchg32RmReg((*s2).try_into()?, s2tmp));
                        }
                    }
                }
                AInstr::Sub(_, _) => todo!(),
                AInstr::Mul(_, _) => todo!(),
                AInstr::Div(_, _) => todo!(),
                AInstr::Mod(_, _) => todo!(),
                AInstr::Return(_) => {
                    let end_stack = mapping.stack_cursor;
                    if let Some(offset) = end_stack.difference(start_stack)? {
                        self.code.insert(0, Instr::Sub64RmImm32(Reg::ESP, offset));
                        self.code.push(Instr::Add64RmImm32(Reg::ESP, offset));
                    }
                    self.code.push(Instr::Xor64RmReg(Reg::EAX.into(), Reg::EAX));
                    self.code.push(Instr::Add8AlImm(0x3c));
                    self.code.push(Instr::Syscall);
                    return Ok(());
                }
            }
        }

        // this should not be reached
        Ok(())
    }

    pub fn encode(&self, buffer: &mut Vec<u8>) {
        for instr in &self.code {
            instr.encode(buffer);
        }
    }

    pub const fn code(&self) -> &[Instr] {
        self.code.as_slice()
    }
}

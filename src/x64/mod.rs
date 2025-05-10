pub mod instr;
pub mod reg;

use instr::Instr;
use reg::{ExtAny, Reg, RegNoExt, Rm32};

use crate::{
    aasm::{AReg, instr::Instr as AInstr, ssa::SsaBlock},
    error::CodeGenError,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

impl CodeGen {
    fn two_way_op(
        &mut self,
        d: impl Into<RegOrStack>,
        s: impl Into<RegOrStack>,
        tmp: Reg<ExtAny>,
        op_rm_reg: impl FnOnce(Rm32, Reg<ExtAny>) -> Instr,
        op_reg_rm: impl FnOnce(Reg<ExtAny>, Rm32) -> Instr,
    ) -> Result<(), CodeGenError> {
        match (d.into(), s.into()) {
            (d, RegOrStack::Reg(s)) => {
                self.code.push(op_rm_reg(d.try_into()?, s));
            }
            (RegOrStack::Reg(d), s) => {
                self.code.push(op_reg_rm(d, s.try_into()?));
            }
            (RegOrStack::Stack(d), RegOrStack::Stack(s)) => {
                self.code.push(Instr::Xchg32RmReg(s.try_into()?, tmp));
                self.code.push(op_rm_reg(d.try_into()?, tmp));
                self.code.push(Instr::Xchg32RmReg(s.try_into()?, tmp));
            }
        }
        Ok(())
    }

    fn three_way_op(
        &mut self,
        d: RegOrStack,
        mut s1: RegOrStack,
        mut s2: RegOrStack,
        tmp: Reg<ExtAny>,
        op_rm_reg: impl FnOnce(Rm32, Reg<ExtAny>) -> Instr,
        op_reg_rm: impl FnOnce(Reg<ExtAny>, Rm32) -> Instr,
    ) -> Result<(), CodeGenError> {
        if d == s2 {
            core::mem::swap(&mut s1, &mut s2);
        } else if d != s1 {
            self.two_way_op(d, s1, tmp, Instr::Mov32RmReg, Instr::Mov32RegRm)?;
        }
        self.two_way_op(d, s2, tmp, op_rm_reg, op_reg_rm)
    }

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
                AInstr::Move(d, s) => {
                    let d = *mapping.get_or_insert(d);
                    let s = *mapping.get_or_insert(s);
                    if d != s {
                        self.two_way_op(d, s, Reg::EAX, Instr::Mov32RmReg, Instr::Mov32RegRm)?;
                    }
                }
                AInstr::Neg(d, s) => {
                    let d = *mapping.get_or_insert(d);
                    let s = *mapping.get_or_insert(s);
                    if d != s {
                        self.two_way_op(d, s, Reg::EAX, Instr::Mov32RmReg, Instr::Mov32RegRm)?;
                    }
                    self.code.push(Instr::Neg32Rm(d.try_into()?));
                }
                AInstr::Add(d, [s1, s2]) => {
                    let d = *mapping.get_or_insert(d);
                    let s1 = *mapping.get_or_insert(s1);
                    let s2 = *mapping.get_or_insert(s2);

                    if let Some((RegOrStack::Reg(d), RegOrStack::Reg(s1), RegOrStack::Reg(s2))) =
                        Some((d, s1, s2)).filter(|(d, s1, s2)| d != s1 && d != s2)
                    {
                        self.code.push(Instr::Lea32(
                            d,
                            Rm32::from_sib(s1, Some(s2), reg::SibMul::Mul1, 0)
                                .ok_or(CodeGenError::InvalidEspAccess)?,
                        ));
                    } else {
                        self.three_way_op(
                            d,
                            s1,
                            s2,
                            Reg::EAX,
                            Instr::Add32RmReg,
                            Instr::Add32RegRm,
                        )?;
                    }
                }
                AInstr::Sub(d, [s1, s2]) => {
                    self.three_way_op(
                        *mapping.get_or_insert(d),
                        *mapping.get_or_insert(s1),
                        *mapping.get_or_insert(s2),
                        Reg::EAX,
                        Instr::Sub32RmReg,
                        Instr::Sub32RegRm,
                    )?;
                }
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

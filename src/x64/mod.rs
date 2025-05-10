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
    freelist: Vec<StackOffset>,
    stack_cursor: StackOffset,
}

impl RegMapping {
    pub fn new(num: usize) -> Self {
        Self {
            map: vec![None; num],
            used_regs: 1 << Reg::ESP.index_full(),
            freelist: vec![],
            stack_cursor: StackOffset { off: 0 },
        }
    }

    pub fn allocmap_reg(&mut self, areg: &AReg, reg: Reg<ExtAny>) -> &RegOrStack {
        self.used_regs |= 1 << reg.index_full();
        self.map[areg.0].insert(reg.into())
    }

    pub fn alloc(&mut self) -> RegOrStack {
        let val = if self.used_regs == u16::MAX {
            RegOrStack::Stack(self.freelist.pop().unwrap_or(self.stack_cursor))
        } else {
            Reg::from_index(self.used_regs.trailing_ones() as _).into()
        };
        match &val {
            RegOrStack::Reg(reg) => {
                self.used_regs |= 1 << reg.index_full();
            }
            RegOrStack::Stack(stack) => {
                self.stack_cursor.off = stack.off + 4;
            }
        }
        val
    }

    pub fn map(&mut self, areg: &AReg, val: RegOrStack) -> &RegOrStack {
        self.map[areg.0].insert(val)
    }

    pub fn free(&mut self, val: RegOrStack) {
        match val {
            RegOrStack::Stack(stack) => self.freelist.push(stack),
            RegOrStack::Reg(reg) => self.used_regs &= !(1 << reg.index_full()),
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

fn free_reg(d: &RegOrStack, s: &RegOrStack) -> Reg<ExtAny> {
    if let RegOrStack::Reg(d) = d {
        *d
    } else if s == &RegOrStack::Reg(Reg::EAX) {
        Reg::EDX
    } else {
        Reg::EAX
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

    fn three_to_two(
        &mut self,
        d: RegOrStack,
        s1: RegOrStack,
        s2: RegOrStack,
        tmp: Reg<ExtAny>,
        order_matters: bool,
    ) -> Result<(RegOrStack, RegOrStack), CodeGenError> {
        if d == s1 {
            Ok((d, s2))
        } else if d == s2 && !order_matters {
            Ok((d, s1))
        } else {
            self.two_way_op(d, s1, tmp, Instr::Mov32RmReg, Instr::Mov32RegRm)?;
            Ok((d, s2))
        }
    }

    fn div_or_mod(
        &mut self,
        d: RegOrStack,
        s1: RegOrStack,
        s2: RegOrStack,
        resultreg: Reg<ExtAny>,
    ) -> Result<(), CodeGenError> {
        let free = |d, c1, c2| match (d, (c1, c2)) {
            (
                RegOrStack::Reg(Reg::EAX | Reg::EDX),
                (RegOrStack::Reg(Reg::EBX), RegOrStack::Reg(Reg::ECX))
                | (RegOrStack::Reg(Reg::ECX), RegOrStack::Reg(Reg::EBX)),
            ) => Some(Reg::ESI),
            (
                RegOrStack::Reg(Reg::EAX | Reg::EDX),
                (RegOrStack::Reg(Reg::EBX), _) | (_, RegOrStack::Reg(Reg::EBX)),
            ) => Some(Reg::ECX),
            (RegOrStack::Reg(Reg::EAX | Reg::EDX), _) => Some(Reg::EBX),
            _ => None,
        };

        let s2save = free(s2, s1, s1);
        let s2tmp = s2save.map(Into::into).unwrap_or(s2);
        if let Some(reg) = s2save {
            self.code.push(Instr::Xchg32RmReg(s2.try_into()?, reg));
        }
        if s1 != RegOrStack::Reg(Reg::EAX) {
            self.code.push(Instr::Mov32RegRm(Reg::EAX, s1.try_into()?));
        }
        if let Some(reg) = free(s1, s2, s2tmp) {
            self.code.push(Instr::Xchg32RmReg(s1.try_into()?, reg));
        }

        self.code.push(Instr::Cdq);
        self.code.push(Instr::Idiv32Rm(s2tmp.try_into()?));
        self.code.push(Instr::Mov32RmReg(d.try_into()?, resultreg));

        if let Some(reg) = free(s1, s2, s2tmp) {
            self.code.push(Instr::Xchg32RmReg(s1.try_into()?, reg));
        }
        if let Some(reg) = s2save {
            self.code.push(Instr::Xchg32RmReg(s2.try_into()?, reg));
        }
        Ok(())
    }

    fn three_way_op(
        &mut self,
        d: RegOrStack,
        s1: RegOrStack,
        s2: RegOrStack,
        tmp: Reg<ExtAny>,
        op_rm_reg: impl FnOnce(Rm32, Reg<ExtAny>) -> Instr,
        op_reg_rm: impl FnOnce(Reg<ExtAny>, Rm32) -> Instr,
        order_matters: bool,
    ) -> Result<(), CodeGenError> {
        let (d, s) = self.three_to_two(d, s1, s2, tmp, order_matters)?;
        self.two_way_op(d, s, tmp, op_rm_reg, op_reg_rm)
    }

    pub fn generate(&mut self, block: &SsaBlock<AReg>) -> Result<(), CodeGenError> {
        let mut mapping = RegMapping::new(block.reg_count());

        if let Some(reg) = block.return_reg() {
            mapping.allocmap_reg(reg, Reg::EDI);
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
                            false,
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
                        true,
                    )?;
                }
                AInstr::Mul(d, [s1, s2]) => {
                    let d = *mapping.get_or_insert(d);
                    let s1 = *mapping.get_or_insert(s1);
                    let s2 = *mapping.get_or_insert(s2);
                    let (d, s) = self.three_to_two(d, s1, s2, Reg::EAX, false)?;
                    let dtmp = free_reg(&d, &s);
                    if d != RegOrStack::Reg(dtmp) {
                        self.code.push(Instr::Xchg32RmReg(d.try_into()?, dtmp));
                    }
                    self.code.push(Instr::Imul32RegRm(dtmp, s.try_into()?));
                    if d != RegOrStack::Reg(dtmp) {
                        self.code.push(Instr::Xchg32RmReg(d.try_into()?, dtmp));
                    }
                }
                AInstr::DivMod([d1, d2], [s1, s2]) => {
                    // TODO: use precolored nodes to not have to push everything
                    let d1 = *mapping.get_or_insert(d1);
                    let d2 = *mapping.get_or_insert(d2);
                    let s1 = *mapping.get_or_insert(s1);
                    let s2 = *mapping.get_or_insert(s2);
                    println!("({d1:?}, {d2:?}) = ({s1:?}, {s2:?})");
                    let s2_is_eax = s2 == RegOrStack::Reg(Reg::EAX);
                    let s2_is_edx = s2 == RegOrStack::Reg(Reg::EDX);
                    let eax_needs_saving = ![d1, d2].contains(&RegOrStack::Reg(Reg::EAX));
                    let edx_needs_saving = ![d1, d2].contains(&RegOrStack::Reg(Reg::EDX));
                    let old_used_reg = mapping.used_regs;
                    mapping.used_regs |=
                        (1 << Reg::EAX.index_full()) | (1 << Reg::EDX.index_full());
                    let eax_tmp = (eax_needs_saving || s2_is_eax).then(|| mapping.alloc());
                    let edx_tmp = (edx_needs_saving || s2_is_edx).then(|| mapping.alloc());
                    mapping.used_regs = old_used_reg;
                    let s2_tmp = edx_tmp
                        .filter(|_| s2_is_edx)
                        .or(eax_tmp.filter(|_| s2_is_eax))
                        .unwrap_or(s2);
                    if let Some(reg) = eax_tmp {
                        self.code
                            .push(Instr::Xchg32RmReg(reg.try_into()?, Reg::EAX));
                    }
                    if let Some(reg) = edx_tmp {
                        self.code
                            .push(Instr::Xchg32RmReg(reg.try_into()?, Reg::EDX));
                    }
                    if s1 != RegOrStack::Reg(Reg::EAX) {
                        self.code.push(Instr::Mov32RegRm(Reg::EAX, s1.try_into()?));
                    }

                    self.code.push(Instr::Cdq);
                    self.code.push(Instr::Idiv32Rm(s2_tmp.try_into()?));
                    let [mut resdiv, mut resmod] = [Reg::EAX, Reg::EDX];

                    if d1 == RegOrStack::Reg(resmod) && d2 == RegOrStack::Reg(resdiv) {
                        self.code
                            .push(Instr::Xchg32RmReg(Reg::EAX.into(), Reg::EDX));
                        core::mem::swap(&mut resdiv, &mut resmod);
                    }
                    if d1 == RegOrStack::Reg(resmod) {
                        if d2 != RegOrStack::Reg(resmod) {
                            self.code.push(Instr::Mov32RmReg(d2.try_into()?, resdiv));
                        }
                        if d1 != RegOrStack::Reg(resmod) {
                            self.code.push(Instr::Mov32RmReg(d1.try_into()?, resmod));
                        }
                    } else {
                        if d1 != RegOrStack::Reg(resmod) {
                            self.code.push(Instr::Mov32RmReg(d1.try_into()?, resdiv));
                        }
                        if d2 != RegOrStack::Reg(resmod) {
                            self.code.push(Instr::Mov32RmReg(d2.try_into()?, resmod));
                        }
                    }

                    if let Some(reg) = edx_tmp {
                        if edx_needs_saving {
                            self.code
                                .push(Instr::Xchg32RmReg(reg.try_into()?, Reg::EDX));
                        }
                        mapping.free(reg);
                    }
                    if let Some(reg) = eax_tmp {
                        if eax_needs_saving {
                            self.code
                                .push(Instr::Xchg32RmReg(reg.try_into()?, Reg::EAX));
                        }
                        mapping.free(reg);
                    }
                }
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

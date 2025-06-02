use crate::ir::{
    BasicBlock, BasicBlockTree,
    instr::{BlockTail, Instr, Label, Op1, Op2, Reg},
};

use super::{
    instrenc::{InstrEnc, ModRm},
    regs::{ColorToRegMap, Reg as ArchReg, RegOrStack},
};

#[derive(Debug, Clone, Default)]
struct LabelOffsetMap {
    map: Vec<u64>,
}

impl LabelOffsetMap {
    fn resize_to_include_index(&mut self, i: Label) {
        if i.0 >= self.map.len() {
            self.map.resize(i.0 + 1, 0);
        }
    }

    pub fn map(&mut self, label: Label, addr: u64) {
        self.resize_to_include_index(label);
        self.map[label.0] = addr;
    }

    pub fn get(&self, label: Label) -> u64 {
        self.map[label.0]
    }
}

#[derive(Debug, Clone, Copy)]
struct LabelFix {
    offset: usize,
    label: Label,
    is_short: bool,
    is_cond: bool,
}

fn remove<T: Copy>(vec: &mut Vec<T>, off: usize, count: usize) {
    vec.copy_within(off.wrapping_add(count).., off);
    vec.truncate(vec.len().wrapping_sub(count));
}

impl LabelFix {
    pub fn offset_size(&self) -> u64 {
        if self.is_short { 1 } else { 4 }
    }

    pub fn src(&self) -> u64 {
        (self.offset as u64).wrapping_add(self.offset_size())
    }

    pub fn delta(&self, map: &LabelOffsetMap) -> i32 {
        let src = self.src();
        let dst = map.get(self.label);
        let delta = dst.wrapping_sub(src).cast_signed();
        i32::try_from(delta).expect("too long jump")
    }
}

#[derive(Default)]
pub struct Codegen {
    code: Vec<u8>,
    label_offset_map: LabelOffsetMap,
    label_fixes: Vec<LabelFix>,
    generated_labels: Vec<Label>,
}

fn translate<const N: usize>(
    arr: [&Reg; N],
    tree: &BasicBlockTree,
    regs: &ColorToRegMap,
) -> [RegOrStack; N] {
    arr.map(|reg| {
        regs.get(tree.inference.vertices[reg.0].color.unwrap())
            .unwrap()
    })
}

impl Codegen {
    pub fn fix_labels(&mut self) {
        for fix in &self.label_fixes {
            let delta = fix.delta(&self.label_offset_map);
            if fix.is_short {
                self.code[fix.offset] = delta as _;
            } else {
                self.code[fix.offset..][..4].copy_from_slice(&delta.to_le_bytes());
            }
        }
    }

    pub fn optimize_jmps(&mut self) {
        while self.optimize_jmps_pass() {}
    }

    pub fn optimize_jmps_pass(&mut self) -> bool {
        let mut change = false;
        let mut fix_index = 0;
        while fix_index < self.label_fixes.len() {
            let fix = &mut self.label_fixes[fix_index];
            if !fix.is_short {
                fix.is_short = true;
                if i8::try_from(fix.delta(&self.label_offset_map)).is_ok() {
                    change = true;
                    let off = fix.offset;
                    remove(&mut self.code, off.wrapping_add(1), 3);
                    let reduction = if fix.is_cond {
                        self.code[off.wrapping_sub(1)] ^= 0xf0;
                        remove(&mut self.code, off.wrapping_sub(2), 1);
                        fix.offset -= 1;
                        4
                    } else {
                        self.code[off.wrapping_sub(1)] = 0xeb;
                        3
                    };
                    for i in fix_index + 1..self.label_fixes.len() {
                        let off = &mut self.label_fixes[i].offset;
                        *off = off.wrapping_sub(reduction);
                    }
                    for i in &mut self.label_offset_map.map {
                        if *i > off as u64 {
                            *i = i.wrapping_sub(reduction as _);
                        }
                    }
                } else {
                    fix.is_short = false;
                }
            }
            let fix = &self.label_fixes[fix_index];
            if fix.is_short && fix.delta(&self.label_offset_map) == 0 {
                let off = fix.offset;
                remove(&mut self.label_fixes, fix_index, 1);
                remove(&mut self.code, off.wrapping_sub(1), 2);
                for i in fix_index..self.label_fixes.len() {
                    let off = &mut self.label_fixes[i].offset;
                    *off = off.wrapping_sub(2);
                }
                for i in &mut self.label_offset_map.map {
                    if *i > off as u64 {
                        *i = i.wrapping_sub(2 as _);
                    }
                }
            }
            fix_index += 1;
        }
        change
    }

    pub fn code(&self) -> &[u8] {
        &self.code
    }

    pub fn gen_from_tree(&mut self, tree: &BasicBlockTree, regs: &ColorToRegMap) {
        let mut work_set = vec![tree.entry];
        let stack_size = regs.stack_size();
        if stack_size > 0 {
            self.enc(
                InstrEnc::new([0x81])
                    .with_modrm(ModRm::from(ArchReg::ESP).with_opext(5))
                    .with_64bit(),
            );
            self.code.extend_from_slice(&stack_size.to_le_bytes());
        }
        while let Some(label) = work_set.pop() {
            self.gen_from_tree_single_label(tree, regs, label, &mut work_set);
        }
    }

    fn gen_from_tree_single_label(
        &mut self,
        tree: &BasicBlockTree,
        regs: &ColorToRegMap,
        label: Label,
        work_set: &mut Vec<Label>,
    ) {
        let block = tree.blocks.get(label.0).unwrap();
        self.label_offset_map.map(label, self.code.len() as _);
        self.gen_from_block(tree, block, regs, work_set);
    }

    fn gen_from_block(
        &mut self,
        tree: &BasicBlockTree,
        block: &BasicBlock,
        regs: &ColorToRegMap,
        work_set: &mut Vec<Label>,
    ) {
        for instr in &block.instrs {
            self.gen_instr(tree, instr, regs);
        }
        if let Some(tail) = &block.tail {
            self.gen_tail(tree, tail, regs, work_set);
        }
    }

    fn enc<const N: usize>(&mut self, enc: InstrEnc<N>) {
        enc.encode(&mut self.code);
    }

    fn gen_move(&mut self, d: impl Into<RegOrStack>, s: impl Into<RegOrStack>) {
        let (d, s) = (d.into(), s.into());
        if d == s {
            return;
        }
        match (d, s) {
            (d, RegOrStack::Reg(s)) => {
                self.enc(InstrEnc::new([0x89]).with_modrm(ModRm::from(d).with_reg(s)))
            }
            (RegOrStack::Reg(d), RegOrStack::Stack(s)) => {
                self.enc(InstrEnc::new([0x8b]).with_modrm(ModRm::from(s).with_reg(d)))
            }
            (RegOrStack::Stack(d), RegOrStack::Stack(s)) => {
                self.gen_move(ArchReg::TMP, s);
                self.gen_move(d, ArchReg::TMP);
            }
        }
    }

    fn gen_cmp(
        &mut self,
        d: &Reg,
        s1: &Reg,
        s2: &Reg,
        tree: &BasicBlockTree,
        regs: &ColorToRegMap,
        op: u8,
    ) {
        let [d, s1, s2] = translate([d, s1, s2], tree, regs);
        match (s1, s2) {
            (s1, RegOrStack::Reg(s2)) => {
                self.enc(InstrEnc::new([0x39]).with_modrm(ModRm::from(s1).with_reg(s2)))
            }
            (RegOrStack::Reg(s1), RegOrStack::Stack(s2)) => {
                self.enc(InstrEnc::new([0x3b]).with_modrm(ModRm::from(s2).with_reg(s1)))
            }
            (RegOrStack::Stack(s1), RegOrStack::Stack(s2)) => {
                self.gen_move(ArchReg::TMP, s2);
                self.enc(InstrEnc::new([0x39]).with_modrm(ModRm::from(s1).with_reg(ArchReg::TMP)))
            }
        }
        self.enc(InstrEnc::new([0x0f, op]).with_modrm(ModRm::from(d).with_byereg()));
    }

    fn gen_shift(
        &mut self,
        d: &Reg,
        s: &Reg,
        tree: &BasicBlockTree,
        regs: &ColorToRegMap,
        op: u8,
        opext: u8,
    ) {
        let [d, _s] = translate([d, s], tree, regs);
        self.enc(InstrEnc::new([op]).with_modrm(ModRm::from(d).with_opext(opext)));
    }

    fn gen_op2(
        &mut self,
        reg_arr: [&Reg; 3],
        tree: &BasicBlockTree,
        regs: &ColorToRegMap,
        is_sym: bool,
        op_rm_reg: u8,
        op_reg_rm: u8,
    ) {
        let [d, mut s1, mut s2] = translate(reg_arr, tree, regs);
        if is_sym && d == s2 {
            core::mem::swap(&mut s1, &mut s2);
        }
        self.gen_move(d, s1);
        // TODO: use `LEA`
        match (d, s2) {
            (d, RegOrStack::Reg(s)) => {
                self.enc(InstrEnc::new([op_rm_reg]).with_modrm(ModRm::from(d).with_reg(s)));
            }
            (RegOrStack::Reg(d), RegOrStack::Stack(s)) => {
                self.enc(InstrEnc::new([op_reg_rm]).with_modrm(ModRm::from(s).with_reg(d)));
            }
            (RegOrStack::Stack(d), RegOrStack::Stack(s)) => {
                self.gen_move(ArchReg::TMP, s);
                self.enc(
                    InstrEnc::new([op_rm_reg]).with_modrm(ModRm::from(d).with_reg(ArchReg::TMP)),
                );
            }
        }
    }

    fn gen_lnot(&mut self, tree: &BasicBlockTree, regs: &ColorToRegMap, d: &Reg, s: &Reg) {
        let [d, s] = translate([d, s], tree, regs);
        self.gen_move(d, s);
        if d == ArchReg::EAX.into() {
            self.code.push(0x34);
        } else {
            self.enc(InstrEnc::new([0x80]).with_modrm(ModRm::from(d).with_opext(6).with_byereg()));
        }
        self.code.push(1);
    }

    fn gen_instr(&mut self, tree: &BasicBlockTree, instr: &Instr, regs: &ColorToRegMap) {
        match instr {
            Instr::Op1(d, s, Op1::BNot) => {
                let [d, s] = translate([d, s], tree, regs);
                self.gen_move(d, s);
                self.enc(InstrEnc::new([0xf7]).with_modrm(ModRm::from(d).with_opext(2)));
            }
            Instr::Op1(d, s, Op1::LNot) => {
                self.gen_lnot(tree, regs, d, s);
            }
            Instr::Op1(d, s, Op1::Neg) => {
                let [d, s] = translate([d, s], tree, regs);
                self.gen_move(d, s);
                self.enc(InstrEnc::new([0xf7]).with_modrm(ModRm::from(d).with_opext(3)));
            }
            Instr::Op2(d, [s1, s2], Op2::Add) => {
                self.gen_op2([d, s1, s2], tree, regs, true, 0x01, 0x03);
            }
            Instr::Op2(d, [s1, s2], Op2::Sub) => {
                self.gen_op2([d, s1, s2], tree, regs, false, 0x29, 0x2b);
            }
            Instr::Op2(d, [s1, s2], Op2::Mul) => {
                let [d, mut s1, mut s2] = translate([d, s1, s2], tree, regs);
                if d == s2 {
                    core::mem::swap(&mut s1, &mut s2);
                }
                if let RegOrStack::Reg(d) = d {
                    self.gen_move(d, s1);
                    self.enc(InstrEnc::new([0x0f, 0xaf]).with_modrm(ModRm::from(s2).with_reg(d)));
                } else {
                    self.gen_move(ArchReg::TMP, s1);
                    self.enc(
                        InstrEnc::new([0x0f, 0xaf])
                            .with_modrm(ModRm::from(s2).with_reg(ArchReg::TMP)),
                    );
                    self.gen_move(d, ArchReg::TMP);
                }
            }
            Instr::Op2(d, [s1, s2], Op2::And) => {
                self.gen_op2([d, s1, s2], tree, regs, true, 0x21, 0x23);
            }
            Instr::Op2(d, [s1, s2], Op2::Xor) => {
                self.gen_op2([d, s1, s2], tree, regs, true, 0x31, 0x33);
            }
            Instr::Op2(d, [s1, s2], Op2::Or) => {
                self.gen_op2([d, s1, s2], tree, regs, true, 0x09, 0x0b);
            }
            Instr::Op2(d, [s, _], Op2::Shl) => self.gen_shift(d, s, tree, regs, 0xd3, 4),
            Instr::Op2(d, [s, _], Op2::Shr) => self.gen_shift(d, s, tree, regs, 0xd3, 7),
            Instr::Op2(d, [s1, s2], Op2::Lt) => self.gen_cmp(d, s1, s2, tree, regs, 0x9c),
            Instr::Op2(d, [s1, s2], Op2::Le) => self.gen_cmp(d, s1, s2, tree, regs, 0x9e),
            Instr::Op2(d, [s1, s2], Op2::Gt) => self.gen_cmp(d, s1, s2, tree, regs, 0x9f),
            Instr::Op2(d, [s1, s2], Op2::Ge) => self.gen_cmp(d, s1, s2, tree, regs, 0x9d),
            Instr::Op2(d, [s1, s2], Op2::IntEq) => self.gen_cmp(d, s1, s2, tree, regs, 0x94),
            Instr::Op2(d, [s1, s2], Op2::IntNe) => self.gen_cmp(d, s1, s2, tree, regs, 0x95),
            Instr::Op2(d, [s1, s2], Op2::BoolEq) => {
                self.gen_op2([d, s1, s2], tree, regs, true, 0x31, 0x33);
                todo!()
            }
            Instr::Op2(d, [s1, s2], Op2::BoolNe) => {
                self.gen_op2([d, s1, s2], tree, regs, true, 0x31, 0x33);
                self.gen_lnot(tree, regs, d, d);
            }
            Instr::DivMod(_, [_, s2]) => {
                let [s2] = translate([s2], tree, regs);
                let mut rs2 = s2;
                if s2 == ArchReg::EDX.into() {
                    rs2 = ArchReg::TMP.into();
                    self.gen_move(ArchReg::TMP, ArchReg::EDX);
                }
                self.code.push(0x99);
                self.enc(InstrEnc::new([0xf7]).with_modrm(ModRm::from(rs2).with_opext(7)));
            }
            Instr::Mov(d, s) => {
                let [d, s] = translate([d, s], tree, regs);
                self.gen_move(d, s);
            }
            Instr::Ld(d, imm) => {
                let [d] = translate([d], tree, regs);
                match d {
                    RegOrStack::Reg(d) => self.enc(InstrEnc::new([0xb8]).with_intreg(d)),
                    RegOrStack::Stack(d) => {
                        self.enc(InstrEnc::new([0xc7]).with_modrm(ModRm::from(d).with_opext(0)))
                    }
                }
                self.code.extend_from_slice(&imm.to_le_bytes());
            }
        }
    }

    fn insert_fix(&mut self, label: Label, work_set: &mut Vec<Label>, is_cond: bool) {
        let offset = self.code.len();
        self.label_fixes.push(LabelFix {
            offset,
            label,
            is_short: false,
            is_cond,
        });
        self.code.extend_from_slice(&[0; 4]);
        if !self.generated_labels.contains(&label) {
            self.generated_labels.push(label);
            work_set.push(label);
        }
    }

    fn gen_jmp(&mut self, label: Label, work_set: &mut Vec<Label>) {
        self.code.push(0xe9);
        self.insert_fix(label, work_set, false);
    }

    fn gen_test_self(&mut self, modrm: impl Into<RegOrStack>) {
        match modrm.into() {
            RegOrStack::Reg(r) => {
                self.enc(
                    InstrEnc::new([0x84]).with_modrm(ModRm::from(r).with_reg(r).with_byereg()),
                );
            }
            RegOrStack::Stack(s) => {
                self.enc(InstrEnc::new([0x80]).with_modrm(ModRm::from(s).with_opext(7)));
                self.code.push(0);
            }
        }
    }

    fn gen_tail(
        &mut self,
        tree: &BasicBlockTree,
        tail: &BlockTail,
        regs: &ColorToRegMap,
        work_set: &mut Vec<Label>,
    ) {
        match tail {
            BlockTail::JmpNoCond(label) => {
                if !work_set.last().is_some_and(|last| last == label) {
                    self.gen_jmp(*label, work_set);
                }
            }
            BlockTail::JmpCond(r, labels) => {
                let [r] = translate([r], tree, regs);
                self.gen_test_self(r);
                self.code.extend_from_slice(&[0x0f, 0x85]);
                self.insert_fix(labels.non_zero, work_set, true);
                if !work_set.last().is_some_and(|last| last == &labels.zero) {
                    self.gen_jmp(labels.zero, work_set);
                }
            }
            BlockTail::Ret(r) => {
                let [r] = translate([r], tree, regs);
                self.gen_move(ArchReg::EDI, r);
                self.code
                    .extend_from_slice(&[0x48, 0x31, 0xc0, 0x04, 0x3c, 0x0f, 0x05]);
            }
        }
    }
}

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

struct LabelFix {
    offset: usize,
    label: Label,
}

pub struct Codegen {
    code: Vec<u8>,
    prog_offset: u64,
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
    pub fn new(prog_offset: u64) -> Self {
        Self {
            code: vec![],
            prog_offset,
            label_offset_map: Default::default(),
            label_fixes: vec![],
            generated_labels: vec![],
        }
    }

    pub fn fix_labels(&mut self) {
        for fix in &self.label_fixes {
            let src = (fix.offset as u64).wrapping_add(4);
            let dst = self.label_offset_map.get(fix.label);
            let delta = dst.wrapping_sub(src).cast_signed();
            let delta = i32::try_from(delta).expect("too long jump");
            self.code[fix.offset..][..4].copy_from_slice(&delta.to_le_bytes());
        }
    }

    pub fn code(&self) -> &[u8] {
        &self.code
    }

    pub fn gen_from_tree(&mut self, tree: &BasicBlockTree, regs: &ColorToRegMap) {
        let mut work_set = vec![tree.entry];
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
                self.enc(InstrEnc::new([0x8b]).with_modrm(ModRm::from(s2).with_reg(s1)))
            }
            (RegOrStack::Stack(s1), RegOrStack::Stack(s2)) => {
                self.gen_move(ArchReg::TMP, s2);
                self.enc(InstrEnc::new([0x39]).with_modrm(ModRm::from(s1).with_reg(ArchReg::TMP)))
            }
        }
        self.enc(InstrEnc::new([0x0f, op]).with_modrm(ModRm::from(d)));
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
        let [d, s] = translate([d, s], tree, regs);
        self.enc(InstrEnc::new([op]).with_modrm(ModRm::from(d).with_opext(opext)));
    }

    fn gen_instr(&mut self, tree: &BasicBlockTree, instr: &Instr, regs: &ColorToRegMap) {
        match instr {
            Instr::Op1(d, s, Op1::BNot) => {
                let [d, s] = translate([d, s], tree, regs);
                self.gen_move(d, s);
                self.enc(InstrEnc::new([0xf7]).with_modrm(ModRm::from(d).with_opext(2)));
            }
            Instr::Op1(d, s, Op1::LNot) => todo!(),
            Instr::Op1(d, s, Op1::Neg) => todo!(),
            Instr::Op2(d, [s1, s2], Op2::Add) => {
                let [d, mut s1, mut s2] = translate([d, s1, s2], tree, regs);
                if d == s2 {
                    core::mem::swap(&mut s1, &mut s2);
                }
                self.gen_move(d, s1);
                // TODO: use `LEA`
                match (d, s2) {
                    (d, RegOrStack::Reg(s)) => {
                        self.enc(InstrEnc::new([0x01]).with_modrm(ModRm::from(d).with_reg(s)));
                    }
                    (RegOrStack::Reg(d), RegOrStack::Stack(s)) => {
                        self.enc(InstrEnc::new([0x03]).with_modrm(ModRm::from(s).with_reg(d)));
                    }
                    (RegOrStack::Stack(d), RegOrStack::Stack(s)) => {
                        self.gen_move(ArchReg::TMP, s);
                        self.enc(
                            InstrEnc::new([0x01]).with_modrm(ModRm::from(d).with_reg(ArchReg::TMP)),
                        );
                    }
                }
            }
            Instr::Op2(d, [s1, s2], Op2::Sub) => todo!(),
            Instr::Op2(d, [s1, s2], Op2::Mul) => todo!(),
            Instr::Op2(d, [s1, s2], Op2::And) => todo!(),
            Instr::Op2(d, [s1, s2], Op2::Xor) => todo!(),
            Instr::Op2(d, [s1, s2], Op2::Or) => todo!(),
            Instr::Op2(d, [s, _], Op2::Shl) => self.gen_shift(d, s, tree, regs, 0xd3, 4),
            Instr::Op2(d, [s, _], Op2::Shr) => self.gen_shift(d, s, tree, regs, 0xd3, 7),
            Instr::Op2(d, [s1, s2], Op2::Lt) => self.gen_cmp(d, s1, s2, tree, regs, 0x9c),
            Instr::Op2(d, [s1, s2], Op2::Le) => self.gen_cmp(d, s1, s2, tree, regs, 0x9e),
            Instr::Op2(d, [s1, s2], Op2::Gt) => self.gen_cmp(d, s1, s2, tree, regs, 0x9f),
            Instr::Op2(d, [s1, s2], Op2::Ge) => self.gen_cmp(d, s1, s2, tree, regs, 0x9d),
            Instr::Op2(d, [s1, s2], Op2::Eq) => self.gen_cmp(d, s1, s2, tree, regs, 0x94),
            Instr::Op2(d, [s1, s2], Op2::Ne) => self.gen_cmp(d, s1, s2, tree, regs, 0x95),
            Instr::DivMod(_, _) => todo!(),
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

    fn insert_fix(&mut self, label: Label, work_set: &mut Vec<Label>) {
        let offset = self.code.len();
        self.label_fixes.push(LabelFix { offset, label });
        self.code.extend_from_slice(&[0; 4]);
        if !self.generated_labels.contains(&label) {
            self.generated_labels.push(label);
            work_set.push(label);
        }
    }

    fn gen_jmp(&mut self, label: Label, work_set: &mut Vec<Label>) {
        self.code.push(0xe9);
        self.insert_fix(label, work_set);
    }

    fn gen_test_self(&mut self, modrm: impl Into<RegOrStack>) {
        match modrm.into() {
            RegOrStack::Reg(r) => {
                self.enc(InstrEnc::new([0x85]).with_modrm(ModRm::from(r).with_reg(r)));
            }
            RegOrStack::Stack(s) => {
                self.enc(InstrEnc::new([0x83]).with_modrm(ModRm::from(s).with_opext(7)));
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
                self.gen_jmp(*label, work_set);
            }
            BlockTail::JmpCond(r, labels) => {
                let [r] = translate([r], tree, regs);
                self.gen_test_self(r);
                self.code.extend_from_slice(&[0x0f, 0x85]);
                self.insert_fix(labels.non_zero, work_set);
                self.gen_jmp(labels.zero, work_set);
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

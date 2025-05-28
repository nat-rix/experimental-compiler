// pub mod color;
pub mod from_ast;
pub mod instr;
pub mod liveness;

use instr::{BlockTail, Instr, Label, Labels, Reg};
use liveness::Graph;

#[derive(Debug, Clone, Default)]
pub struct RegSet {
    items: Vec<Reg>,
}

impl RegSet {
    pub fn remove(&mut self, reg: Reg) {
        if let Some(pos) = self.items.iter().position(|r| r == &reg) {
            self.items.swap_remove(pos);
        }
    }

    /// Inserts `reg` into the set and returns if the set changed
    pub fn insert(&mut self, reg: Reg) -> bool {
        let is_free = !self.items.contains(&reg);
        if is_free {
            self.items.push(reg);
        }
        is_free
    }

    pub fn union(&mut self, rhs: &Self) -> bool {
        let mut changed = false;
        for i in &rhs.items {
            changed |= self.insert(*i);
        }
        changed
    }
}

#[derive(Debug, Clone, Default)]
pub struct Annotation {
    pub live_vars: RegSet,
}

#[derive(Debug, Clone, Default)]
pub struct BasicBlock {
    pub instrs: Vec<Instr>,
    pub tail: Option<BlockTail>,
    pub xrefs: Vec<Label>,
    pub annotations: Vec<Annotation>,
    pub live_out: RegSet,
}

impl BasicBlock {
    pub fn instr_count(&self) -> usize {
        self.instrs.len() + self.tail.is_some() as usize
    }

    pub fn get_reg_split(&mut self, i: usize) -> (&mut [Reg], &mut [Reg]) {
        if i >= self.instrs.len() {
            self.tail.as_mut().unwrap().split_regs_mut()
        } else {
            self.instrs[i].split_regs_mut()
        }
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlockTree {
    pub blocks: Vec<BasicBlock>,
    pub entry: Label,
    pub alloc: from_ast::RegAlloc,
    pub inference: Graph,
}

impl BasicBlockTree {
    pub fn new_fun() -> Self {
        Self {
            blocks: vec![Default::default()],
            entry: Label(0),
            alloc: Default::default(),
            inference: Default::default(),
        }
    }

    pub fn alloc_block(&mut self) -> Label {
        let label = Label(self.blocks.len());
        self.blocks.push(Default::default());
        label
    }

    pub fn get_mut(&mut self, label: Label) -> &mut BasicBlock {
        &mut self.blocks[label.0]
    }
}

impl core::fmt::Display for BasicBlockTree {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        for (i, block) in self.blocks.iter().enumerate() {
            let label = Label(i);
            write!(f, "label{i}:")?;
            if label == self.entry {
                write!(f, " <entry>")?;
            }
            writeln!(f)?;
            writeln!(f, "{}", block.display(4))?;
        }
        Ok(())
    }
}

impl core::fmt::Display for RegSet {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        for (i, r) in self.items.iter().enumerate() {
            if i != 0 {
                write!(f, ",")?;
            }
            write!(f, "{r}")?;
        }
        Ok(())
    }
}
impl BasicBlock {
    pub fn display(&self, indent: usize) -> BasicBlockDisplay {
        BasicBlockDisplay(self, indent)
    }
    fn display_live(&self, f: &mut core::fmt::Formatter, i: usize) -> core::fmt::Result {
        let Some(ann) = self.annotations.get(i) else {
            return Ok(());
        };
        write!(f, " | (alive: {})", ann.live_vars)
    }
}
pub struct BasicBlockDisplay<'a>(&'a BasicBlock, usize);
impl<'a> core::fmt::Display for BasicBlockDisplay<'a> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        for (i, instr) in self.0.instrs.iter().enumerate() {
            write!(f, "{}", instr.display(self.1))?;
            self.0.display_live(f, i)?;
            writeln!(f)?;
        }
        if let Some(tail) = &self.0.tail {
            write!(f, "{}", tail.display(self.1))?;
            self.0.display_live(f, self.0.instrs.len())?;
            writeln!(f)?;
        }
        if !self.0.live_out.items.is_empty() {
            indent(f, self.1)?;
            writeln!(f, "live out: {}", self.0.live_out)?;
        }
        Ok(())
    }
}

fn indent(f: &mut core::fmt::Formatter, indent: usize) -> core::fmt::Result {
    for _ in 0..indent {
        write!(f, " ")?;
    }
    Ok(())
}

impl Instr {
    pub fn display(&self, indent: usize) -> InstrDisplay {
        InstrDisplay(self, indent)
    }
}
pub struct InstrDisplay<'a>(&'a Instr, usize);
impl<'a> core::fmt::Display for InstrDisplay<'a> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        indent(f, self.1)?;
        match self.0 {
            Instr::Op1(dst, src, op) => write!(
                f,
                "mov {dst}, {}{src}",
                match op {
                    instr::Op1::BNot => "~",
                    instr::Op1::LNot => "!",
                    instr::Op1::Neg => "-",
                }
            ),
            Instr::Op2(dst, [src1, src2], op2) => write!(
                f,
                "{} {dst}, {src1}, {src2}",
                match op2 {
                    instr::Op2::Add => "add",
                    instr::Op2::Sub => "sub",
                    instr::Op2::Mul => "mul",
                    instr::Op2::And => "and",
                    instr::Op2::Xor => "xor",
                    instr::Op2::Or => "or",
                    instr::Op2::Shl => "shl",
                    instr::Op2::Shr => "shr",
                    instr::Op2::Lt => "lt",
                    instr::Op2::Le => "le",
                    instr::Op2::Gt => "gt",
                    instr::Op2::Ge => "ge",
                    instr::Op2::Eq => "eq",
                    instr::Op2::Ne => "ne",
                }
            ),
            Instr::DivMod([dst1, dst2], [src1, src2]) => {
                write!(f, "divmod {dst1}, {dst2}, {src1}, {src2}")
            }
            Instr::Mov(dst, src) => write!(f, "mov {dst}, {src}"),
            Instr::Ld(dst, imm) => write!(f, "mov {dst}, 0x{imm:x}"),
        }
    }
}

impl BlockTail {
    pub fn display(&self, indent: usize) -> BlockTailDisplay {
        BlockTailDisplay(self, indent)
    }
}
pub struct BlockTailDisplay<'a>(&'a BlockTail, usize);
impl<'a> core::fmt::Display for BlockTailDisplay<'a> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        indent(f, self.1)?;
        match &self.0 {
            BlockTail::JmpCond(cond, Labels { non_zero, zero }) => {
                write!(f, "jmp {cond} ? [label{}] : [label{}]", non_zero.0, zero.0)
            }
            BlockTail::JmpNoCond(label) => {
                write!(f, "jmp [label{}]", label.0)
            }
            BlockTail::Ret(src) => write!(f, "ret {src}"),
        }
    }
}

impl core::fmt::Display for Reg {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "r{}", self.0)
    }
}

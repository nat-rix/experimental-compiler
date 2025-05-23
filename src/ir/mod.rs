pub mod from_ast;
pub mod instr;

use instr::{Instr, Label, Reg};

#[derive(Debug, Clone, Default)]
pub struct BasicBlock {
    pub instrs: Vec<Instr>,
}

#[derive(Debug, Clone)]
pub struct BasicBlockTree {
    pub blocks: Vec<BasicBlock>,
    pub entry: Label,
}

impl BasicBlockTree {
    pub fn new_fun() -> Self {
        Self {
            blocks: vec![Default::default()],
            entry: Label(0),
        }
    }

    pub fn alloc_block(&mut self) -> Label {
        let label = Label(self.blocks.len());
        self.blocks.push(Default::default());
        label
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

impl BasicBlock {
    pub fn display(&self, indent: usize) -> BasicBlockDisplay {
        BasicBlockDisplay(self, indent)
    }
}
pub struct BasicBlockDisplay<'a>(&'a BasicBlock, usize);
impl<'a> core::fmt::Display for BasicBlockDisplay<'a> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        for instr in &self.0.instrs {
            writeln!(f, "{}", instr.display(self.1))?;
        }
        Ok(())
    }
}

impl Instr {
    pub fn display(&self, indent: usize) -> InstrDisplay {
        InstrDisplay(self, indent)
    }
}
pub struct InstrDisplay<'a>(&'a Instr, usize);
impl<'a> core::fmt::Display for InstrDisplay<'a> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        for _ in 0..self.1 {
            write!(f, " ")?;
        }
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
            Instr::Jmp(cond, label) => {
                match cond {
                    instr::Cond::NotZero(reg) => write!(f, "jmp {reg}, ")?,
                    instr::Cond::Zero(reg) => write!(f, "jmp !{reg}, ")?,
                    instr::Cond::None => write!(f, "jmp ")?,
                }
                write!(f, "[label{}]", label.0)
            }
            Instr::Ret(src) => write!(f, "ret {src}"),
        }
    }
}

impl core::fmt::Display for Reg {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "r{}", self.0)
    }
}

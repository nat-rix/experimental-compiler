use super::regs::Reg as ArchReg;
use crate::ir::{
    BasicBlockTree,
    from_ast::RegAlloc,
    instr::{BlockTail, Instr, Op2, Reg},
    liveness::Graph,
};

#[derive(Debug, Default)]
pub struct Precolors {
    pub(super) eax: Option<usize>,
    pub(super) ecx: Option<usize>,
    pub(super) edx: Option<usize>,
    pub(super) edi: Option<usize>,
}

impl Precolors {
    pub fn iter(&self) -> impl Iterator<Item = (ArchReg, usize)> {
        [
            (ArchReg::EAX, self.eax),
            (ArchReg::ECX, self.ecx),
            (ArchReg::EDX, self.edx),
            (ArchReg::EDI, self.edi),
        ]
        .into_iter()
        .filter_map(|(r, c)| c.map(|c| (r, c)))
    }
}

fn precolorize_dst(
    alloc: &mut RegAlloc,
    reg: &mut Reg,
    color: usize,
    coloring: &mut Graph,
    postpend: &mut Vec<Instr>,
) {
    if coloring.colorize_conflicting(*reg, color) {
        let old_reg = *reg;
        *reg = alloc.alloc();
        coloring.colorize_conflicting(*reg, color);
        postpend.push(Instr::Mov(old_reg, *reg));
    }
}

fn precolorize_src(
    alloc: &mut RegAlloc,
    reg: &mut Reg,
    color: usize,
    coloring: &mut Graph,
    prepend: &mut Vec<Instr>,
) {
    if coloring.colorize_conflicting(*reg, color) {
        let old_reg = *reg;
        *reg = alloc.alloc();
        coloring.colorize_conflicting(*reg, color);
        prepend.push(Instr::Mov(*reg, old_reg));
    }
}

pub fn precolorize(tree: &mut BasicBlockTree) -> Precolors {
    let mut color_count = 0;
    let mut colors = Precolors::default();
    let mut alloc = |c: &mut Option<usize>| {
        *c.get_or_insert_with(|| {
            let next = color_count + 1;
            core::mem::replace(&mut color_count, next)
        })
    };
    let mut postpend = vec![];
    let mut prepend = vec![];
    for block in &mut tree.blocks {
        let mut cursor = 0;
        while let Some(instr) = &mut block.instrs.get_mut(cursor) {
            postpend.clear();
            prepend.clear();
            match instr {
                Instr::Op2(_, [_, s2], Op2::Shl | Op2::Shr) => {
                    let ecx = alloc(&mut colors.ecx);
                    precolorize_src(&mut tree.alloc, s2, ecx, &mut tree.inference, &mut prepend);
                }
                Instr::DivMod([d1, d2], [s1, _]) => {
                    let eax = alloc(&mut colors.eax);
                    let edx = alloc(&mut colors.edx);
                    precolorize_dst(&mut tree.alloc, d1, eax, &mut tree.inference, &mut postpend);
                    precolorize_dst(&mut tree.alloc, d2, edx, &mut tree.inference, &mut postpend);
                    precolorize_src(&mut tree.alloc, s1, eax, &mut tree.inference, &mut prepend);
                }
                _ => (),
            }
            for i in &prepend {
                block.instrs.insert(cursor, *i);
                cursor += 1;
            }
            for i in &postpend {
                block.instrs.insert(cursor + 1, *i);
            }
            cursor += 1;
        }
        prepend.clear();
        if let Some(BlockTail::Ret(s)) = &mut block.tail {
            let edi = alloc(&mut colors.edi);
            precolorize_src(&mut tree.alloc, s, edi, &mut tree.inference, &mut prepend);
        }
        for i in &prepend {
            block.instrs.insert(cursor, *i);
            cursor += 1;
        }
    }
    colors
}

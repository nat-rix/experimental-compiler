use super::{
    AReg,
    instr::Instr,
    ssa::{Lifetimes, SsaBlock},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PreColor {
    Eax,
    Edx,
    Edi,
}

#[derive(Debug, Clone, Default)]
pub struct Precolorizer {
    pre_colors: Vec<PreColor>,
}

impl Precolorizer {
    fn alloc_color(&mut self, pre_color: PreColor) -> usize {
        if let Some(color) = self.pre_colors.iter().position(|p| p == &pre_color) {
            color
        } else {
            let color = self.pre_colors.len();
            self.pre_colors.push(pre_color);
            color
        }
    }

    pub fn pre_colors(&self) -> impl Iterator<Item = (AReg, PreColor)> {
        self.pre_colors
            .iter()
            .copied()
            .enumerate()
            .map(|(i, c)| (AReg(i), c))
    }

    pub fn make_block_precolorizable(&mut self, block: &mut SsaBlock<AReg>) {
        let mut cursor = 0;
        while let Some(instr) = block.code.get_mut(cursor) {
            let (dst, src) = split_instr_mut(instr);
            let dst_instrs = dst.map_collect(|(reg, _)| {
                let new_reg = block.alloc.alloc();
                let old_reg = core::mem::replace(reg, new_reg);
                Instr::MoveR(old_reg, new_reg)
            });
            let src_instrs = src.map_collect(|(reg, _)| {
                let new_reg = block.alloc.alloc();
                let old_reg = core::mem::replace(reg, new_reg);
                Instr::MoveR(new_reg, old_reg)
            });
            for src_instr in src_instrs {
                block.code.insert(cursor, src_instr);
                cursor += 1;
            }
            for dst_instr in dst_instrs {
                cursor += 1;
                block.code.insert(cursor, dst_instr);
            }
            cursor += 1;
        }
    }

    pub fn precolorize(&mut self, block: &SsaBlock<AReg>, lifetimes: &mut Lifetimes) {
        for instr in &block.code {
            let (dst, src) = split_instr(instr);
            for (reg, pre_color) in dst.chain(src) {
                lifetimes.precolorize(reg.0, self.alloc_color(pre_color));
            }
        }
    }
}

struct Iter2<T>(Option<(T, Option<T>)>);

impl<T> Iter2<T> {
    pub const fn new2(a: T, b: T) -> Self {
        Self(Some((a, Some(b))))
    }

    pub const fn new1(a: T) -> Self {
        Self(Some((a, None)))
    }

    pub fn map_collect<U>(self, mut f: impl FnMut(T) -> U) -> Iter2<U> {
        Iter2(self.0.map(|(a, m)| (f(a), m.map(f))))
    }
}

impl<T> Iterator for Iter2<T> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        let (a, m) = self.0.take()?;
        Some(if let Some(m) = m {
            self.0 = Some((a, None));
            m
        } else {
            a
        })
    }
}

macro_rules! impl_split_instr {
    ($instr:ident) => {{
        use PreColor::*;
        match $instr {
            Instr::DivModRR([d1, d2], [s1, _]) | Instr::DivModRI([d1, d2], s1, _) => {
                (Iter2::new2((d1, Eax), (d2, Edx)), Iter2::new1((s1, Eax)))
            }
            Instr::DivModIR([d1, d2], _, _) => (Iter2::new2((d1, Eax), (d2, Edx)), Iter2(None)),
            Instr::ReturnR(s) => (Iter2(None), Iter2::new1((s, Edi))),
            _ => (Iter2(None), Iter2(None)),
        }
    }};
}

type Split<T> = (T, T);

fn split_instr(instr: &Instr<AReg>) -> Split<Iter2<(&AReg, PreColor)>> {
    impl_split_instr!(instr)
}

fn split_instr_mut(instr: &mut Instr<AReg>) -> Split<Iter2<(&mut AReg, PreColor)>> {
    impl_split_instr!(instr)
}

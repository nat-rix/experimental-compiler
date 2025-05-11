use super::{AReg, ARegAlloc, Instr};

/// Inclusive range over instructions
#[derive(Debug, Clone, Copy)]
pub struct Lifetime {
    pub start: usize,
    pub end: usize,
    color: usize,
}

impl Lifetime {
    pub const fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end,
            color: 0,
        }
    }

    pub const fn collides(&self, rhs: &Self) -> bool {
        (self.start < rhs.end && self.end > rhs.start) || self.start == rhs.start
    }
}

#[derive(Debug, Clone)]
pub struct SsaBlock<R> {
    code: Vec<Instr<R>>,
    reg_count: usize,
}

impl<R> SsaBlock<R> {
    pub fn code(&self) -> &[Instr<R>] {
        &self.code
    }
}

impl SsaBlock<AReg> {
    pub fn from_code(mut code: Vec<Instr<AReg>>, mut alloc: ARegAlloc) -> Self {
        // do ssa renaming
        for i in (0..code.len()).rev() {
            let [first @ .., last] = &mut code[..=i] else {
                unreachable!()
            };
            let (last_dst, last_src) = last.split_regs_dst_src_mut();
            for reg in last_dst {
                let mut new_reg = None;
                let mut new_reg_getter = || *new_reg.get_or_insert_with(|| alloc.alloc());
                last_src
                    .iter_mut()
                    .filter(|r| *r == reg)
                    .for_each(|r| *r = new_reg_getter());
                for instr in first.iter_mut() {
                    instr.replace_all(reg, &mut new_reg_getter);
                }
            }
        }
        Self {
            code,
            reg_count: alloc.count(),
        }
    }

    pub fn rename_from_colors(&mut self, lifetimes: &Lifetimes, color_count: usize) {
        self.reg_count = color_count;
        for instr in &mut self.code {
            for reg in instr.regs_mut() {
                reg.0 = lifetimes.lifetimes[reg.0].color;
            }
        }
    }
}

impl<R> SsaBlock<R> {
    pub fn return_reg(&self) -> Option<&R> {
        self.code.iter().find_map(|instr| match instr {
            Instr::ReturnR(r) => Some(r),
            _ => None,
        })
    }

    pub const fn reg_count(&self) -> usize {
        self.reg_count
    }
}

#[derive(Debug, Clone)]
pub struct Lifetimes {
    lifetimes: Vec<Lifetime>,
}

impl Lifetimes {
    pub fn as_slice(&self) -> &[Lifetime] {
        self.lifetimes.as_slice()
    }

    pub fn from_block(block: &SsaBlock<AReg>) -> Self {
        let mut lifetimes = vec![Lifetime::new(0, 0); block.reg_count];
        for (i, instr) in block.code.iter().enumerate().rev() {
            let (dst, srcs) = instr.split_regs_dst_src();
            for dst in dst {
                let lifetime = &mut lifetimes[dst.0];
                lifetime.start = i;
                if lifetime.end == 0 {
                    // this happens only if this register is unused
                    lifetime.end = i;
                }
            }
            for src in srcs {
                let lifetime = &mut lifetimes[src.0];
                if lifetime.end == 0 {
                    lifetime.end = i;
                }
            }
        }
        Self { lifetimes }
    }

    /// Create a graph coloring for the intervals and return the count of colors
    pub fn colorize(&mut self) -> usize {
        let ordering = self.create_ordering();
        let mut count = 0;
        for i in 0..ordering.len() {
            let lifetime = self.lifetimes[ordering[i]];
            let neighbors = || {
                ordering
                    .iter()
                    .take(i)
                    .map(|j| &self.lifetimes[*j])
                    .filter(|lt| lt.collides(&lifetime))
                    .map(|n| n.color)
            };
            let mut color = 0;
            while neighbors().any(|c| c == color) {
                color += 1;
            }
            count = count.max(color + 1);
            self.lifetimes[ordering[i]].color = color;
        }
        count
    }

    fn create_ordering(&self) -> Vec<usize> {
        let mut ordering: Vec<_> = (0..self.lifetimes.len()).collect();
        // interval graph can be ordered like this
        ordering.sort_unstable_by_key(|i| self.lifetimes[*i].start);
        ordering
    }
}

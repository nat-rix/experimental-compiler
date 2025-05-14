use super::{AReg, ARegAlloc, Instr};

/// Inclusive range over instructions
#[derive(Debug, Clone, Copy)]
pub struct Lifetime {
    pub start: usize,
    pub end: usize,
    color: usize,
    is_precolored: bool,
}

impl Lifetime {
    pub const fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end,
            color: 0,
            is_precolored: false,
        }
    }

    pub const fn precolorize(&mut self, color: usize) {
        self.color = color;
        self.is_precolored = true;
    }

    pub const fn collides(&self, rhs: &Self) -> bool {
        (self.start < rhs.end && self.end > rhs.start) || self.start == rhs.start
    }
}

#[derive(Debug, Clone)]
pub struct SsaBlock<R> {
    pub(super) code: Vec<Instr<R>>,
    pub(super) alloc: ARegAlloc,
}

impl<R> SsaBlock<R> {
    pub fn code(&self) -> &[Instr<R>] {
        &self.code
    }

    pub(super) fn code_mut(&mut self) -> &mut Vec<Instr<R>> {
        &mut self.code
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
        Self { code, alloc }
    }

    pub fn constant_propagation(&mut self) {
        super::cprop::ConstantPropagation::new(self).propagate();
    }

    pub fn eliminate_unused_calculations(&mut self) {
        let mut eliminated = true;
        while eliminated {
            eliminated = false;
            let mut cursor = 0;
            while let Some(instr) = self.code.get(cursor).copied() {
                let is_unused = instr.is_side_effect_free()
                    && instr.dst_regs().iter().all(|d| {
                        self.code[cursor + 1..]
                            .iter()
                            .all(|instr| !instr.src_regs().contains(d))
                    });
                if is_unused {
                    self.code.remove(cursor);
                    eliminated = true;
                    continue;
                }
                cursor += 1;
            }
        }
    }

    pub fn eliminate_self_moves(&mut self) {
        self.code
            .retain(|i| !matches!(i, Instr::MoveR(dst, src) if dst == src));
    }

    pub fn rename_from_colors(&mut self, lifetimes: &Lifetimes, color_count: usize) {
        self.set_reg_count(color_count);
        for instr in &mut self.code {
            for reg in instr.regs_mut() {
                reg.0 = lifetimes.lifetimes[reg.0].color;
                self.alloc.0.0 = self.alloc.0.0.max(reg.0 + 1);
            }
        }
    }
}

impl<R> SsaBlock<R> {
    pub const fn reg_count(&self) -> usize {
        self.alloc.0.0
    }

    pub const fn set_reg_count(&mut self, count: usize) {
        self.alloc.0.0 = count;
    }
}

#[derive(Debug, Clone)]
pub struct Lifetimes {
    lifetimes: Vec<Lifetime>,
    color_count: usize,
}

impl Lifetimes {
    pub fn as_slice(&self) -> &[Lifetime] {
        self.lifetimes.as_slice()
    }

    pub(super) fn precolorize(&mut self, lifetime_index: usize, color: usize) {
        let lifetime = &mut self.lifetimes[lifetime_index];
        lifetime.color = color;
        lifetime.is_precolored = true;
        self.color_count = self.color_count.max(color + 1);
    }

    pub fn from_block(block: &SsaBlock<AReg>) -> Self {
        let mut lifetimes = vec![Lifetime::new(0, 0); block.reg_count()];
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
        Self {
            lifetimes,
            color_count: 0,
        }
    }

    /// Create a graph coloring for the intervals and return the count of colors
    pub fn colorize(&mut self) -> usize {
        let ordering = self.create_ordering();
        for i in 0..ordering.len() {
            let lifetime = &self.lifetimes[ordering[i]];
            if lifetime.is_precolored {
                continue;
            }
            let neighbors = || {
                ordering
                    .iter()
                    .take(i)
                    .map(|j| &self.lifetimes[*j])
                    .filter(|lt| lt.collides(lifetime))
                    .map(|n| n.color)
            };
            let mut color = 0;
            while neighbors().any(|c| c == color) {
                color += 1;
            }
            self.color_count = self.color_count.max(color + 1);
            self.lifetimes[ordering[i]].color = color;
        }
        self.color_count
    }

    fn create_ordering(&self) -> Vec<usize> {
        let mut ordering: Vec<_> = (0..self.lifetimes.len()).collect();
        // interval graph can be ordered like this
        ordering.sort_unstable_by_key(|i| {
            let lifetime = &self.lifetimes[*i];
            (!lifetime.is_precolored, lifetime.start)
        });
        ordering
    }

    pub fn coalesc(&mut self, block: &SsaBlock<AReg>) {
        for instr in block.code() {
            if let Instr::MoveR(dst, src) = instr {
                self.coalesc_single_move(dst, src);
            }
        }
    }

    fn coalesc_single_move(&mut self, dst: &AReg, src: &AReg) {
        let dst_lifetime = &self.lifetimes[dst.0];
        let src_lifetime = &self.lifetimes[src.0];
        if (src_lifetime.is_precolored && dst_lifetime.is_precolored)
            || src_lifetime.collides(dst_lifetime)
        {
            return;
        }

        let check_collide = |color| {
            self.lifetimes.iter().enumerate().any(|(i, lt)| {
                lt.color == color
                    && i != dst.0
                    && i != src.0
                    && (lt.collides(dst_lifetime) || lt.collides(src_lifetime))
            })
        };

        let mut color = 0;
        if !src_lifetime.is_precolored && !dst_lifetime.is_precolored {
            while check_collide(color) {
                color += 1
            }
        } else {
            color = if src_lifetime.is_precolored {
                src_lifetime.color
            } else {
                dst_lifetime.color
            };
            if check_collide(color) {
                return;
            }
        }

        self.lifetimes[dst.0].color = color;
        self.lifetimes[src.0].color = color;
        self.color_count = self.color_count.max(color + 1);
    }
}

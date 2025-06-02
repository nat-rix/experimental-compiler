use crate::ir::BasicBlockTree;

use super::precolor::Precolors;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Reg(u8);

impl Reg {
    pub const EAX: Self = Self(0);
    pub const ECX: Self = Self(1);
    pub const EDX: Self = Self(2);
    pub const EBX: Self = Self(3);
    pub const ESP: Self = Self(4);
    pub const EBP: Self = Self(5);
    pub const ESI: Self = Self(6);
    pub const EDI: Self = Self(7);
    pub const R8: Self = Self(8);
    pub const R9: Self = Self(9);
    pub const R10: Self = Self(10);
    pub const R11: Self = Self(11);
    pub const R12: Self = Self(12);
    pub const R13: Self = Self(13);
    pub const R14: Self = Self(14);
    pub const R15: Self = Self(15);

    pub const TMP: Self = Self::R11;

    pub const fn index(&self) -> u8 {
        self.0
    }

    pub const fn with_msb_of(self, rhs: Self) -> Self {
        Self((self.0 & 7) | (rhs.0 & 8))
    }

    pub const fn msb(&self) -> bool {
        self.0 & 8 != 0
    }

    pub const fn is_resverved(&self) -> bool {
        matches!(*self, Self::TMP | Self::ESP)
    }
}

impl From<u8> for Reg {
    fn from(value: u8) -> Self {
        Self(value & 15)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct StackOff(pub i32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RegOrStack {
    Reg(Reg),
    Stack(StackOff),
}

impl From<Reg> for RegOrStack {
    fn from(value: Reg) -> Self {
        Self::Reg(value)
    }
}

impl From<StackOff> for RegOrStack {
    fn from(value: StackOff) -> Self {
        Self::Stack(value)
    }
}

#[derive(Debug, Clone, Default)]
pub struct ColorToRegMap {
    map: Vec<Option<RegOrStack>>,
    next_stack_offset: StackOff,
}

impl ColorToRegMap {
    fn resize_to_include_index(&mut self, i: usize) {
        if i >= self.map.len() {
            self.map.resize(i + 1, None);
        }
    }

    pub fn map(&mut self, color: usize, reg: impl Into<RegOrStack>) {
        self.resize_to_include_index(color);
        let reg = reg.into();
        self.map[color] = Some(reg);
        if let RegOrStack::Stack(off) = reg {
            self.next_stack_offset.0 = self.next_stack_offset.0.max(off.0 + 4);
        }
    }

    fn find_free_reg(&mut self) -> RegOrStack {
        for i in 0..15 {
            if Reg(i).is_resverved() {
                continue;
            }
            let reg = Reg(i).into();
            if !self.map.contains(&Some(reg)) {
                return reg;
            }
        }
        let off = self.next_stack_offset;
        self.next_stack_offset.0 += 4;
        off.into()
    }

    pub fn get(&self, color: usize) -> Option<RegOrStack> {
        *self.map.get(color)?
    }

    pub fn populate_from_tree(&mut self, tree: &BasicBlockTree) {
        // TODO: some smarter algorithm
        for v in &tree.inference.vertices {
            if let Some(color) = v.color {
                if self.get(color).is_some() {
                    continue;
                }
                let reg = self.find_free_reg();
                self.map(color, reg);
            }
        }
    }

    pub fn stack_size(&self) -> u32 {
        self.map
            .iter()
            .filter_map(|i| i.as_ref())
            .map(|v| match v {
                RegOrStack::Reg(_) => 0,
                RegOrStack::Stack(off) => off.0.max(0).cast_unsigned().saturating_add(4),
            })
            .max()
            .unwrap_or(0)
    }
}

impl From<Precolors> for ColorToRegMap {
    fn from(value: Precolors) -> Self {
        let mut slf = Self::default();
        for (reg, color) in value.iter() {
            slf.map(color, reg);
        }
        slf
    }
}

use super::Instr;

#[derive(Debug, Clone)]
pub struct SsaBlock<R> {
    code: Vec<Instr<R>>,
}

impl<R: Clone + Eq> SsaBlock<R> {
    pub fn from_code(mut code: Vec<Instr<R>>, mut alloc: impl FnMut() -> R) -> Self {
        // do ssa renaming
        for i in (0..code.len()).rev() {
            let [first @ .., last] = &mut code[..=i] else {
                unreachable!()
            };
            let Some(reg) = last.dst_reg() else {
                continue;
            };
            let mut new_reg = None;
            let mut new_reg_getter = || new_reg.get_or_insert_with(&mut alloc).clone();
            for instr in first.iter_mut() {
                instr.replace_all(reg, &mut new_reg_getter);
            }
        }
        Self { code }
    }
}

impl<R> SsaBlock<R> {
    pub fn code(&self) -> &[Instr<R>] {
        &self.code
    }
}

use super::{AReg, ssa::SsaBlock};
use crate::aasm::instr::Instr;

use std::collections::HashMap;

pub struct ConstantPropagation<'a> {
    block: &'a mut SsaBlock<AReg>,
    values: HashMap<AReg, i32>,
    cursor: usize,
}

impl<'a> ConstantPropagation<'a> {
    pub fn new(block: &'a mut SsaBlock<AReg>) -> Self {
        Self {
            block,
            values: Default::default(),
            cursor: 0,
        }
    }

    pub fn propagate(&mut self) {
        while let Some(instr) = self.block.code().get(self.cursor).copied() {
            self.propagate_step(instr);
            self.cursor += 1;
        }
    }

    fn get_value(&self, reg: &AReg) -> Option<i32> {
        self.values.get(reg).copied()
    }

    fn get_value_res<'reg>(&self, reg: &'reg AReg) -> Result<i32, &'reg AReg> {
        self.values.get(reg).copied().ok_or(reg)
    }

    fn set_value(&mut self, reg: &AReg, val: Option<i32>) {
        if let Some(val) = val {
            self.values.insert(*reg, val);
        } else {
            self.values.remove(reg);
        }
    }

    fn set_value_and_instr(&mut self, reg: &AReg, val: Option<i32>) {
        self.set_value(reg, val);
        if let Some(val) = val {
            let instr = &mut self.block.code_mut()[self.cursor];
            *instr = Instr::MoveI(*reg, val);
        }
    }

    fn propagate_step_d1s2(
        &mut self,
        d: &AReg,
        s1: Result<i32, &AReg>,
        s2: Result<i32, &AReg>,
        f: impl FnOnce(Option<i32>, Option<i32>) -> Option<i32>,
        f_imm_reg: impl FnOnce(AReg, i32, AReg) -> Instr<AReg>,
        f_reg_imm: impl FnOnce(AReg, AReg, i32) -> Instr<AReg>,
    ) {
        let res = f(s1.ok(), s2.ok());
        self.set_value_and_instr(d, res);
        if res.is_none() {
            match (s1, s2) {
                (Ok(imm1), Err(s2)) => {
                    self.block.code_mut()[self.cursor] = f_imm_reg(*d, imm1, *s2)
                }
                (Err(s1), Ok(imm2)) => {
                    self.block.code_mut()[self.cursor] = f_reg_imm(*d, *s1, imm2)
                }
                _ => (),
            }
        }
    }

    fn propagate_step_d2s2(
        &mut self,
        [d1, d2]: [AReg; 2],
        s1: Result<i32, &AReg>,
        s2: Result<i32, &AReg>,
        f: impl FnOnce(Option<i32>, Option<i32>) -> Result<(Option<i32>, Option<i32>), Instr<AReg>>,
        f_imm_reg: impl FnOnce([AReg; 2], i32, AReg) -> Instr<AReg>,
        f_reg_imm: impl FnOnce([AReg; 2], AReg, i32) -> Instr<AReg>,
    ) {
        let (res1, res2) = match f(s1.ok(), s2.ok()) {
            Ok(v) => v,
            Err(instr) => {
                self.block.code_mut()[self.cursor] = instr;
                return;
            }
        };
        self.set_value(&d1, res1);
        self.set_value(&d2, res2);
        if let (Some(res1), Some(res2)) = (res1, res2) {
            self.block.code_mut()[self.cursor] = Instr::MoveI(d2, res2);
            self.block
                .code_mut()
                .insert(self.cursor, Instr::MoveI(d1, res1));
        } else {
            match (s1, s2) {
                (Ok(imm1), Err(s2)) => {
                    self.block.code_mut()[self.cursor] = f_imm_reg([d1, d2], imm1, *s2)
                }
                (Err(s1), Ok(imm2)) => {
                    self.block.code_mut()[self.cursor] = f_reg_imm([d1, d2], *s1, imm2)
                }
                _ => (),
            }
        }
    }

    fn propagate_step_add(&mut self, d: &AReg, s1: Result<i32, &AReg>, s2: Result<i32, &AReg>) {
        self.propagate_step_d1s2(
            d,
            s1,
            s2,
            |a, b| a.zip(b).map(|(a, b)| a.wrapping_add(b)),
            |d, i, s| Instr::AddRI(d, s, i),
            Instr::AddRI,
        );
    }

    fn propagate_step_sub(&mut self, d: &AReg, s1: Result<i32, &AReg>, s2: Result<i32, &AReg>) {
        self.propagate_step_d1s2(
            d,
            s1,
            s2,
            |a, b| a.zip(b).map(|(a, b)| a.wrapping_sub(b)),
            Instr::SubIR,
            |d, s, i| Instr::AddRI(d, s, i.wrapping_neg()),
        );
    }

    fn propagate_step_mul(&mut self, d: &AReg, s1: Result<i32, &AReg>, s2: Result<i32, &AReg>) {
        self.propagate_step_d1s2(
            d,
            s1,
            s2,
            |a, b| match (a, b) {
                (Some(a), Some(b)) => Some(a.wrapping_mul(b)),
                (Some(0), _) | (_, Some(0)) => Some(0),
                _ => None,
            },
            Instr::SubIR,
            |d, s, i| Instr::AddRI(d, s, i.wrapping_neg()),
        );
    }

    fn propagate_step_divmod(
        &mut self,
        ds: [AReg; 2],
        s1: Result<i32, &AReg>,
        s2: Result<i32, &AReg>,
    ) {
        self.propagate_step_d2s2(
            ds,
            s1,
            s2,
            |a, b| {
                Ok(match (a, b) {
                    (_, Some(0)) | (Some(i32::MIN), Some(-1)) => {
                        return Err(Instr::FailFloatingPoint);
                    }
                    (Some(a), Some(b)) => (Some(a / b), Some(a % b)),
                    _ => (None, None),
                })
            },
            Instr::DivModIR,
            Instr::DivModRI,
        );
    }

    fn propagate_step(&mut self, instr: Instr<AReg>) {
        match instr {
            Instr::MoveR(d, s) => {
                let imm = self.get_value(&s);
                self.set_value_and_instr(&d, imm);
            }
            Instr::MoveI(d, imm) => {
                self.set_value_and_instr(&d, Some(imm));
            }
            Instr::NegR(d, s) => {
                let imm = self.get_value(&s).map(|imm| imm.wrapping_neg());
                self.set_value_and_instr(&d, imm);
            }
            Instr::AddRR(d, [s1, s2]) => {
                self.propagate_step_add(&d, self.get_value_res(&s1), self.get_value_res(&s2));
            }
            Instr::AddRI(d, s1, imm2) => {
                self.propagate_step_add(&d, self.get_value_res(&s1), Ok(imm2));
            }
            Instr::SubRR(d, [s1, s2]) => {
                self.propagate_step_sub(&d, self.get_value_res(&s1), self.get_value_res(&s2));
            }
            Instr::SubIR(d, imm1, s2) => {
                self.propagate_step_sub(&d, Ok(imm1), self.get_value_res(&s2));
            }
            Instr::IMulRR(d, [s1, s2]) => {
                self.propagate_step_mul(&d, self.get_value_res(&s1), self.get_value_res(&s2));
            }
            Instr::IMulRI(d, s1, imm2) => {
                self.propagate_step_mul(&d, self.get_value_res(&s1), Ok(imm2));
            }
            Instr::DivModRR(ds, [s1, s2]) => {
                self.propagate_step_divmod(ds, self.get_value_res(&s1), self.get_value_res(&s2));
            }
            Instr::DivModRI(ds, s1, imm2) => {
                self.propagate_step_divmod(ds, self.get_value_res(&s1), Ok(imm2));
            }
            Instr::DivModIR(ds, imm1, s2) => {
                self.propagate_step_divmod(ds, Ok(imm1), self.get_value_res(&s2));
            }
            Instr::ReturnR(s) => {
                if let Some(imm) = self.get_value(&s) {
                    self.block.code_mut()[self.cursor] = Instr::ReturnI(imm);
                }
            }
            Instr::ReturnI(_) => (),
            Instr::FailFloatingPoint => (),
        }
    }
}

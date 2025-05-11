use crate::{aasm::instr::Instr, parser::ast::IntLit};

use super::CodeGen;

#[derive(Debug, Clone)]
enum PropagationResult {
    Fail,
    Return(i32),
}

pub struct ConstantPropagation<'a, 'b> {
    code_gen: &'b mut CodeGen<'a>,
    values: Vec<i32>,
}

impl<'a, 'b> ConstantPropagation<'a, 'b> {
    pub fn new(code_gen: &'b mut CodeGen<'a>) -> Self {
        Self {
            code_gen,
            values: vec![],
        }
    }

    pub fn propagate(&mut self) {
        // TODO
        return;
        let res = self.get_result();
        match res {
            PropagationResult::Fail => todo!(),
            PropagationResult::Return(val) => {
                todo!()
            }
        }
    }

    pub fn get_result(&mut self) -> PropagationResult {
        for instr in &self.code_gen.code {
            match instr {
                Instr::LoadConst(reg, IntLit(val)) => todo!(),
                Instr::Move(_, _) => todo!(),
                Instr::Neg(_, _) => todo!(),
                Instr::Add(_, _) => todo!(),
                Instr::Sub(_, _) => todo!(),
                Instr::Mul(_, _) => todo!(),
                Instr::DivMod(_, _) => todo!(),
                Instr::Return(_) => todo!(),
            }
        }
        todo!()
    }
}

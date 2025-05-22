use crate::error::AnaError;

use super::{Block, Ctrl, Function, Stmt};

pub fn return_check<'a>(func: &Function<'a>) -> Result<(), AnaError<'a>> {
    if !func.block.check_return() {
        return Err(AnaError::ReturnCheck(
            func.return_ty.span().combine(func.block.span),
        ));
    }
    Ok(())
}

trait CheckReturn {
    fn check_return(&self) -> bool;
}

impl<'a> CheckReturn for Block<'a> {
    fn check_return(&self) -> bool {
        self.stmts.iter().any(|stmt| stmt.check_return())
    }
}

impl<'a> CheckReturn for Stmt<'a> {
    fn check_return(&self) -> bool {
        match self {
            Stmt::Simp(_) => false,
            Stmt::Ctrl(v) => v.check_return(),
            Stmt::Block(v) => v.check_return(),
        }
    }
}

impl<'a> CheckReturn for Ctrl<'a> {
    fn check_return(&self) -> bool {
        match self {
            Self::If(v) => v
                .else_block
                .as_ref()
                .is_some_and(|else_block| v.stmt.check_return() && else_block.stmt.check_return()),
            Self::While(_) | Self::For(_) | Self::Continue(_) | Self::Break(_) => false,
            Self::Return(_) => true,
        }
    }
}

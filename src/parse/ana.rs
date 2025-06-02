use crate::error::AnaError;

use super::{Ast, TypeName, tokenize::Ident, type_check::TypeCheck};

pub fn check_full<'a>(ast: &mut Ast<'a>) -> Result<(), AnaError<'a>> {
    check_main_fun(ast)?;
    ast.type_check(())?;
    Ok(())
}

pub fn check_main_fun<'a>(ast: &Ast<'a>) -> Result<(), AnaError<'a>> {
    if *ast.main_fun.name != Ident(b"main") {
        return Err(AnaError::OnlyMainFunction(ast.main_fun.name));
    }
    if !matches!(ast.main_fun.return_ty, TypeName::Int(_)) {
        return Err(AnaError::MainMustReturnInt {
            ty: ast.main_fun.return_ty,
            ident: ast.main_fun.name,
        });
    }
    Ok(())
}

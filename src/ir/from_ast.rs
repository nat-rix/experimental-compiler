use super::{
    BasicBlockTree, Label,
    instr::{BlockTail, Instr, Reg},
};
use crate::{
    error::CodeGenError,
    ir::instr::{Labels, Op1, Op2},
    parse::{
        Asgn, AsgnOp, Ast, Block, BoolConst, Ctrl, CtrlBreak, CtrlContinue, CtrlIf, Expr, ExprAtom,
        ExprOp1, Function, Simp, Stmt, tokenize::Ident,
    },
};

#[derive(Debug, Clone)]
pub struct RegAlloc(Reg);

impl Default for RegAlloc {
    fn default() -> Self {
        Self(Reg(0))
    }
}

impl RegAlloc {
    pub fn alloc(&mut self) -> Reg {
        let res = self.0;
        self.0.0 += 1;
        res
    }

    pub fn alloc_count(&self) -> usize {
        self.0.0
    }
}

#[derive(Debug, Clone)]
struct Var<'a> {
    ident: Ident<'a>,
    reg: Reg,
}

#[derive(Debug, Clone, Default)]
struct Vars<'a> {
    vars: Vec<Var<'a>>,
}

impl<'a> Vars<'a> {
    pub fn get(&self, ident: &Ident) -> Result<&Var<'a>, CodeGenError> {
        self.vars
            .iter()
            .find(|v| &v.ident == ident)
            .ok_or(CodeGenError::InvalidVarAccess)
    }
}

#[derive(Debug, Clone)]
struct LoopContext {
    continue_label: Label,
    break_label: Label,
}

#[derive(Debug)]
struct Context<'a, 't> {
    vars: Vars<'a>,
    tree: &'t mut BasicBlockTree,
    label: Label,
    loop_stack: Vec<LoopContext>,
}

impl<'a, 't> Context<'a, 't> {
    pub fn insert_instr(&mut self, instr: Instr) {
        let block = &mut self.tree.blocks[self.label.0];
        block.instrs.push(instr)
    }

    pub fn insert_jmp(&mut self, label: Label) {
        self.insert_tail(BlockTail::JmpNoCond(label));
        self.tree.blocks[label.0].xrefs.push(self.label);
    }

    pub fn insert_condjmp(&mut self, cond: Reg, non_zero: Label, zero: Label) {
        self.insert_tail(BlockTail::JmpCond(cond, Labels { non_zero, zero }));
        self.tree.blocks[non_zero.0].xrefs.push(self.label);
        self.tree.blocks[zero.0].xrefs.push(self.label);
    }

    pub fn insert_tail(&mut self, tail: BlockTail) {
        let block = &mut self.tree.blocks[self.label.0];
        if block.tail.is_none() {
            block.tail = Some(tail);
        }
    }
}

pub fn generete_ir_from_ast(ast: &Ast) -> Result<BasicBlockTree, CodeGenError> {
    gen_fun(&ast.main_fun)
}

fn gen_fun(ast: &Function) -> Result<BasicBlockTree, CodeGenError> {
    let mut tree = BasicBlockTree::new_fun();
    let mut ctx = Context {
        vars: Default::default(),
        label: tree.entry,
        tree: &mut tree,
        loop_stack: vec![],
    };
    gen_block(&ast.block, &mut ctx)?;
    Ok(tree)
}

fn gen_block<'a>(block: &Block<'a>, ctx: &mut Context<'a, '_>) -> Result<bool, CodeGenError> {
    for stmt in &block.stmts {
        if gen_stmt(stmt, ctx)? {
            return Ok(true);
        }
    }
    Ok(false)
}

fn gen_stmt<'a>(stmt: &Stmt<'a>, ctx: &mut Context<'a, '_>) -> Result<bool, CodeGenError> {
    match stmt {
        Stmt::Simp(simp) => gen_simp(&simp.simp, ctx).map(|_| false),
        Stmt::Ctrl(ctrl) => gen_ctrl(ctrl, ctx),
        Stmt::Block(block) => gen_block(block, ctx),
    }
}

fn gen_simp<'a>(simp: &Simp<'a>, ctx: &mut Context<'a, '_>) -> Result<(), CodeGenError> {
    match simp {
        Simp::Asgn(asgn) => gen_asgn(asgn, ctx)?,
        Simp::Decl(decl) => {
            gen_decl_raw(&decl.name, ctx)?;
            if let Some(asgn) = &decl.val.val {
                gen_asgn_raw(&decl.name, &asgn.expr, ctx)?;
            }
        }
    }
    Ok(())
}

fn gen_asgn<'a>(asgn: &Asgn<'a>, ctx: &mut Context<'a, '_>) -> Result<(), CodeGenError> {
    let op = match asgn.op {
        AsgnOp::Asgn(_) => return gen_asgn_raw(&asgn.lvalue, &asgn.expr, ctx),
        AsgnOp::Div(_) => return gen_divmod_asgn(&asgn.lvalue, &asgn.expr, false, ctx),
        AsgnOp::Rem(_) => return gen_divmod_asgn(&asgn.lvalue, &asgn.expr, true, ctx),
        AsgnOp::Sub(_) => Op2::Sub,
        AsgnOp::Add(_) => Op2::Add,
        AsgnOp::Mul(_) => Op2::Mul,
        AsgnOp::And(_) => Op2::And,
        AsgnOp::Shl(_) => Op2::Shl,
        AsgnOp::Shr(_) => Op2::Shr,
        AsgnOp::Or(_) => Op2::Or,
        AsgnOp::Xor(_) => Op2::Xor,
    };
    let lhs_reg = ctx.vars.get(&asgn.lvalue)?.reg;
    let rhs_reg = gen_expr(&asgn.expr, ctx)?;
    ctx.insert_instr(Instr::Op2(lhs_reg, [lhs_reg, rhs_reg], op));
    Ok(())
}

fn gen_decl_raw<'a>(ident: &Ident<'a>, ctx: &mut Context<'a, '_>) -> Result<(), CodeGenError> {
    ctx.vars.vars.push(Var {
        ident: *ident,
        reg: ctx.tree.alloc.alloc(),
    });
    Ok(())
}

fn gen_asgn_raw<'a>(
    ident: &Ident<'a>,
    expr: &Expr<'a>,
    ctx: &mut Context<'a, '_>,
) -> Result<(), CodeGenError> {
    let dst_reg = ctx.vars.get(ident)?.reg;
    let src_reg = gen_expr(expr, ctx)?;
    ctx.insert_instr(Instr::Mov(dst_reg, src_reg));
    Ok(())
}

fn gen_expr<'a>(expr: &Expr<'a>, ctx: &mut Context<'a, '_>) -> Result<Reg, CodeGenError> {
    match expr {
        Expr::Atom(atom) => gen_atom(atom, ctx),
        Expr::Op1(op) => gen_op1(op, ctx),
        Expr::Op2(lhs, op, rhs) => gen_op2(lhs, op, rhs, ctx),
        Expr::Op3(cond, _, lhs, _, rhs) => {
            let cond_reg = gen_expr(cond, ctx)?;
            let then_block = ctx.tree.alloc_block();
            let else_block = ctx.tree.alloc_block();
            let after_block = ctx.tree.alloc_block();
            ctx.insert_condjmp(cond_reg, then_block, else_block);
            ctx.label = then_block;
            let lhs_reg = gen_expr(lhs, ctx)?;
            ctx.insert_jmp(after_block);
            ctx.label = else_block;
            let rhs_reg = gen_expr(rhs, ctx)?;
            ctx.insert_instr(Instr::Mov(lhs_reg, rhs_reg));
            ctx.insert_jmp(after_block);
            ctx.label = after_block;
            Ok(lhs_reg)
        }
    }
}

fn gen_op2<'a>(
    lhs: &Expr<'a>,
    op: &crate::parse::Op2,
    rhs: &Expr<'a>,
    ctx: &mut Context<'a, '_>,
) -> Result<Reg, CodeGenError> {
    use crate::parse::Op2::*;
    let op = match op {
        Add(_) => Op2::Add,
        Sub(_) => Op2::Sub,
        Mul(_) => Op2::Mul,
        Lt(_) => Op2::Lt,
        Le(_) => Op2::Le,
        Gt(_) => Op2::Gt,
        Ge(_) => Op2::Ge,
        Eq(_) => Op2::Eq,
        Neq(_) => Op2::Ne,
        BAnd(_) => Op2::And,
        BXor(_) => Op2::Xor,
        BOr(_) => Op2::Or,
        Shl(_) => Op2::Shl,
        Shr(_) => Op2::Shr,

        Div(_) => return gen_divmod(lhs, rhs, false, ctx),
        Rem(_) => return gen_divmod(lhs, rhs, true, ctx),

        LAnd(_) => {
            let after_block = ctx.tree.alloc_block();
            let else_block = ctx.tree.alloc_block();
            let reg = gen_expr(lhs, ctx)?;
            ctx.insert_condjmp(reg, else_block, after_block);
            ctx.label = else_block;
            let reg_rhs = gen_expr(rhs, ctx)?;
            ctx.insert_instr(Instr::Mov(reg, reg_rhs));
            ctx.insert_jmp(after_block);
            ctx.label = after_block;
            return Ok(reg);
        }
        LOr(_) => {
            let after_block = ctx.tree.alloc_block();
            let else_block = ctx.tree.alloc_block();
            let reg = gen_expr(lhs, ctx)?;
            ctx.insert_condjmp(reg, after_block, else_block);
            ctx.label = else_block;
            let reg_rhs = gen_expr(rhs, ctx)?;
            ctx.insert_instr(Instr::Mov(reg, reg_rhs));
            ctx.insert_jmp(after_block);
            ctx.label = after_block;
            return Ok(reg);
        }
    };
    let lhs_reg = gen_expr(lhs, ctx)?;
    let rhs_reg = gen_expr(rhs, ctx)?;
    ctx.insert_instr(Instr::Op2(lhs_reg, [lhs_reg, rhs_reg], op));
    Ok(lhs_reg)
}

fn gen_divmod<'a>(
    lhs: &Expr<'a>,
    rhs: &Expr<'a>,
    is_rem: bool,
    ctx: &mut Context<'a, '_>,
) -> Result<Reg, CodeGenError> {
    let lhs_reg = gen_expr(lhs, ctx)?;
    let rhs_reg = gen_expr(rhs, ctx)?;
    let div_reg = ctx.tree.alloc.alloc();
    let rem_reg = ctx.tree.alloc.alloc();
    ctx.insert_instr(Instr::DivMod([div_reg, rem_reg], [lhs_reg, rhs_reg]));
    Ok(if is_rem { rem_reg } else { div_reg })
}

fn gen_divmod_asgn<'a>(
    lhs: &Ident<'a>,
    rhs: &Expr<'a>,
    is_rem: bool,
    ctx: &mut Context<'a, '_>,
) -> Result<(), CodeGenError> {
    let lhs_reg = ctx.vars.get(lhs)?.reg;
    let rhs_reg = gen_expr(rhs, ctx)?;
    let tmp_reg = ctx.tree.alloc.alloc();
    let dst = if is_rem {
        [tmp_reg, lhs_reg]
    } else {
        [lhs_reg, tmp_reg]
    };
    ctx.insert_instr(Instr::DivMod(dst, [lhs_reg, rhs_reg]));
    Ok(())
}

fn gen_op1<'a>(op: &ExprOp1<'a>, ctx: &mut Context<'a, '_>) -> Result<Reg, CodeGenError> {
    use crate::parse::Op1::*;
    let reg = gen_expr(&op.expr, ctx)?;
    let op = match op.op {
        BNot(_) => Op1::BNot,
        LNot(_) => Op1::LNot,
        Neg(_) => Op1::Neg,
    };
    ctx.insert_instr(Instr::Op1(reg, reg, op));
    Ok(reg)
}

fn gen_atom<'a>(atom: &ExprAtom<'a>, ctx: &mut Context<'a, '_>) -> Result<Reg, CodeGenError> {
    match atom {
        ExprAtom::BoolConst(c) => gen_imm(matches!(c, BoolConst::True(_)) as _, ctx),
        ExprAtom::IntConst(c) => gen_imm(**c, ctx),
        ExprAtom::Ident(ident) => {
            let reg = ctx.tree.alloc.alloc();
            let var = ctx.vars.get(ident)?;
            ctx.insert_instr(Instr::Mov(reg, var.reg));
            Ok(reg)
        }
    }
}

fn gen_imm<'a>(val: i32, ctx: &mut Context<'a, '_>) -> Result<Reg, CodeGenError> {
    let reg = ctx.tree.alloc.alloc();
    ctx.insert_instr(Instr::Ld(reg, val));
    Ok(reg)
}

fn gen_ctrl<'a>(ctrl: &Ctrl<'a>, ctx: &mut Context<'a, '_>) -> Result<bool, CodeGenError> {
    (match ctrl {
        Ctrl::If(ctrl) => gen_if(ctrl, ctx),
        Ctrl::While(ctrl) => gen_for_raw(None, &ctrl.cond, None, &ctrl.stmt, ctx),
        Ctrl::For(ctrl) => gen_for_raw(
            ctrl.op0.as_ref(),
            &ctrl.op1,
            ctrl.op2.as_ref(),
            &ctrl.stmt,
            ctx,
        ),
        Ctrl::Continue(ctrl) => gen_continue(ctrl, ctx),
        Ctrl::Break(ctrl) => gen_break(ctrl, ctx),
        Ctrl::Return(ret) => {
            let reg = gen_expr(&ret.expr, ctx)?;
            ctx.insert_tail(BlockTail::Ret(reg));
            return Ok(true);
        }
    })?;
    Ok(false)
}

fn gen_if<'a>(ctrl: &CtrlIf<'a>, ctx: &mut Context<'a, '_>) -> Result<(), CodeGenError> {
    let cond_reg = gen_expr(&ctrl.cond, ctx)?;
    let after_label = ctx.tree.alloc_block();
    let then_label = ctx.tree.alloc_block();
    let else_label = ctrl
        .else_block
        .as_ref()
        .map(|_| ctx.tree.alloc_block())
        .unwrap_or(after_label);

    ctx.insert_condjmp(cond_reg, then_label, else_label);
    ctx.label = then_label;
    gen_stmt(&ctrl.stmt, ctx)?;
    ctx.insert_jmp(after_label);

    if let Some(else_block) = &ctrl.else_block {
        ctx.label = else_label;
        gen_stmt(&else_block.stmt, ctx)?;
        ctx.insert_jmp(after_label);
    }

    ctx.label = after_label;
    Ok(())
}

fn gen_for_raw<'a>(
    init: Option<&Simp<'a>>,
    cond: &Expr<'a>,
    step: Option<&Simp<'a>>,
    block: &Stmt<'a>,
    ctx: &mut Context<'a, '_>,
) -> Result<(), CodeGenError> {
    if let Some(init) = &init {
        gen_simp(init, ctx)?;
    }
    let block_label = ctx.tree.alloc_block();
    let cond_label = ctx.tree.alloc_block();
    let step_label = step.map(|_| ctx.tree.alloc_block()).unwrap_or(cond_label);
    let after_label = ctx.tree.alloc_block();

    ctx.insert_jmp(cond_label);
    ctx.loop_stack.push(LoopContext {
        continue_label: step_label,
        break_label: after_label,
    });

    ctx.label = cond_label;
    let cond_reg = gen_expr(cond, ctx)?;
    ctx.insert_condjmp(cond_reg, block_label, after_label);

    ctx.label = block_label;
    gen_stmt(block, ctx)?;
    ctx.insert_jmp(step_label);

    if let Some(step) = step {
        ctx.label = step_label;
        gen_simp(step, ctx)?;
        ctx.insert_jmp(cond_label);
    }

    ctx.label = after_label;
    let _ = ctx.loop_stack.pop();
    Ok(())
}

fn gen_continue(_ctrl: &CtrlContinue, ctx: &mut Context) -> Result<(), CodeGenError> {
    ctx.insert_jmp(
        ctx.loop_stack
            .last()
            .ok_or(CodeGenError::InvalidContinue)?
            .continue_label,
    );
    Ok(())
}

fn gen_break(_ctrl: &CtrlBreak, ctx: &mut Context) -> Result<(), CodeGenError> {
    ctx.insert_jmp(
        ctx.loop_stack
            .last()
            .ok_or(CodeGenError::InvalidBreak)?
            .break_label,
    );
    Ok(())
}

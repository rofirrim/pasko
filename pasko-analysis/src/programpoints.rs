use std::collections::HashMap;
use std::num::NonZero;

use crate::visitor;
use crate::visitor::Visitable;

use pasko_frontend::ast::{self, Program, StmtAssignment};
use pasko_frontend::span::{SpanId, SpannedBox};

use std::fmt::{Display, Error, Formatter};

type ProgramCounter = usize;

pub struct ProgramPoints<'a> {
    // Sequence of AST trees expressing the order of the program.
    // Some of those trees will contain branches, so we need to map them again.
    program_points: Vec<ProgramPoint<'a>>,
}

pub enum ProgramPoint<'a> {
    Assig(SpanId, &'a ast::Assig),
    Stmt(SpanId, &'a ast::Stmt),
    Expr(SpanId, &'a ast::Expr),
    BranchIf(SpanId, &'a ast::Expr, ProgramCounter),
    Branch(ProgramCounter),
    NoOperation, // Can we remove this?
}

impl<'a> ProgramPoints<'a> {
    fn new() -> ProgramPoints<'a> {
        let program_points = Vec::new();
        ProgramPoints { program_points }
    }

    fn add_assig(&mut self, id: SpanId, assig: &'a ast::Assig) {
        self.program_points.push(ProgramPoint::Assig(id, assig))
    }

    fn add_expr(&mut self, id: SpanId, expr: &'a ast::Expr) {
        self.program_points.push(ProgramPoint::Expr(id, expr))
    }

    fn add_stmt(&mut self, id: SpanId, stmt: &'a ast::Stmt) {
        self.program_points.push(ProgramPoint::Stmt(id, stmt))
    }

    // Adds a goto to fixup later
    #[must_use]
    fn add_goto(&mut self) -> usize {
        self.program_points.push(ProgramPoint::Branch(0));

        self.program_points.len() - 1
    }

    // Adds a conditional goto to fixup later
    #[must_use]
    fn add_goto_if(&mut self, id: SpanId, expr: &'a ast::Expr) -> usize {
        self.program_points
            .push(ProgramPoint::BranchIf(id, expr, 0));

        self.program_points.len() - 1
    }

    fn get_next_point(&mut self) -> usize {
        return self.program_points.len();
    }

    fn set_branch_target(&mut self, pc: ProgramCounter, target_pc: ProgramCounter) {
        // FIXME: We can make the push operations above to overwrite added nops.
        if pc == self.program_points.len() {
            self.program_points.push(ProgramPoint::NoOperation);
        }
        assert!(pc < self.program_points.len(), "Invalid program point");
        match &mut self.program_points[pc] {
            ProgramPoint::Branch(x) => {
                *x = target_pc;
            }
            ProgramPoint::BranchIf(_, _, x) => {
                *x = target_pc;
            }
            _ => {
                panic!("Instruction is not a branch");
            }
        }
    }
}

impl<'a> ProgramPoint<'a> {
    fn fmt_expr(&self, f: &mut Formatter<'_>, expr: &ast::Expr) -> Result<(), Error> {
        match &expr {
            &ast::Expr::BinOp(ast::ExprBinOp(op, lhs, rhs)) => {
                write!(
                    f,
                    "${:05} {} ${:05}",
                    lhs.id().get_number(),
                    op.get(),
                    rhs.id().get_number()
                )?;
            }
            &ast::Expr::UnOp(ast::ExprUnOp(op, operand)) => {
                write!(f, "{} ${:05}", op.get(), operand.id().get_number())?;
            }
            &ast::Expr::Variable(ast::ExprVariable(assig)) => {
                write!(f, "load ${:05}", assig.id().get_number())?;
            }
            &ast::Expr::VariableReference(ast::ExprVariableReference(assig)) => {
                write!(f, "${:05}", assig.id().get_number())?;
            }
            &ast::Expr::Const(ast::ExprConst(my_const)) => match my_const.get() {
                ast::Const::Integer(x) => {
                    write!(f, "{}", x.0.get())?;
                }
                ast::Const::Real(x) => {
                    write!(f, "{}", x.0.get())?;
                }
                _ => {
                    write!(f, "{:?}", my_const)?;
                }
            },
            _ => {
                write!(f, "{:?}", expr)?;
            }
        }
        Ok(())
    }

    fn fmt_stmt(&self, f: &mut Formatter<'_>, stmt: &ast::Stmt) -> Result<(), Error> {
        match &stmt {
            &ast::Stmt::Assignment(StmtAssignment(lhs, rhs)) => {
                write!(
                    f,
                    "store ${:05} into ${:05}",
                    rhs.id().get_number(),
                    lhs.id().get_number()
                )?;
            }
            &ast::Stmt::ProcedureCall(ast::StmtProcedureCall(proc_name, args)) => {
                write!(f, "call {}(", proc_name.get())?;
                if let Some(args) = args {
                    for (idx, arg) in args.iter().enumerate() {
                        if idx > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "${:05}", arg.id().get_number())?;
                    }
                }
                write!(f, ")")?;
            }
            _ => {
                write!(f, "{:?}", stmt)?;
            }
        }
        Ok(())
    }

    fn fmt_assig(&self, f: &mut Formatter<'_>, assig: &ast::Assig) -> Result<(), Error> {
        match &assig {
            &ast::Assig::Variable(ast::AssigVariable(var)) => {
                write!(f, "@{}", var.get())?;
            }
            _ => {
                write!(f, "{:?}", assig)?;
            }
        }
        Ok(())
    }
}

impl<'a> Display for ProgramPoint<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            ProgramPoint::Stmt(id, stmt) => {
                self.fmt_stmt(f, *stmt)?;
            }
            ProgramPoint::Expr(id, expr) => {
                write!(f, "${:05} ← ", id.get_number())?;
                self.fmt_expr(f, *expr)?;
            }
            ProgramPoint::Assig(id, assig) => {
                write!(f, "${:05} ← ", id.get_number())?;
                self.fmt_assig(f, assig)?;
            }
            ProgramPoint::BranchIf(id, _expr, target) => {
                write!(f, "branch if ${:05} to @{:05}", id.get_number(), *target)?;
            }
            ProgramPoint::Branch(target) => {
                write!(f, "branch to @{:05}", *target)?;
            }
            ProgramPoint::NoOperation => {
                write!(f, "nop")?;
            }
        }
        Ok(())
    }
}

pub struct ProgramPointsBuilder<'a> {
    points: ProgramPoints<'a>,
}

impl<'a> Display for ProgramPoints<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        for (pc, point) in self.program_points.iter().enumerate() {
            writeln!(f, "[@{:05}] {}", pc, point)?
        }
        Ok(())
    }
}

impl<'a> ProgramPointsBuilder<'a> {
    pub fn new() -> ProgramPointsBuilder<'a> {
        let points = ProgramPoints::new();
        ProgramPointsBuilder { points }
    }

    pub fn build(mut self, stmt: &'a SpannedBox<ast::Stmt>) -> ProgramPoints<'a> {
        stmt.get().walk_mut(&mut self, stmt.loc(), stmt.id());

        self.points
    }
}

impl<'a> visitor::VisitorMut<'a> for ProgramPointsBuilder<'a> {
    fn visit_post_assig(
        &mut self,
        n: &'a ast::Assig,
        _span: &pasko_frontend::span::SpanLoc,
        id: pasko_frontend::span::SpanId,
    ) {
        self.points.add_assig(id, n);
    }

    fn visit_post_expr(
        &mut self,
        n: &'a ast::Expr,
        _span: &pasko_frontend::span::SpanLoc,
        id: pasko_frontend::span::SpanId,
    ) {
        self.points.add_expr(id, n);
    }

    fn visit_pre_stmt(
        &mut self,
        n: &'a ast::Stmt,
        _span: &pasko_frontend::span::SpanLoc,
        id: pasko_frontend::span::SpanId,
    ) -> bool {
        match &n {
            &ast::Stmt::Compound(stmt_compound) => {
                for stmt in &stmt_compound.0 {
                    stmt.get().walk_mut(self, stmt.loc(), stmt.id());
                }
                return false;
            }
            &ast::Stmt::If(stmt_if) => {
                // Foo
                let cond_expr = &stmt_if.0;
                let then_stmt = &stmt_if.1;
                let else_stmt = &stmt_if.2;
                // Execute condition
                self.points.add_expr(cond_expr.id(), cond_expr.get());

                if let Some(else_stmt) = else_stmt {
                    // Branch to then block.
                    let then_branch = self.points.add_goto_if(cond_expr.id(), cond_expr.get());
                    // If not branch to else block.
                    let else_branch = self.points.add_goto();

                    // Execute then block.
                    let then_target = self.points.get_next_point();
                    then_stmt
                        .get()
                        .walk_mut(self, then_stmt.loc(), then_stmt.id());
                    self.points.set_branch_target(then_branch, then_target);
                    // Branch to end block.
                    let end_branch = self.points.add_goto();

                    // Execute the else block.
                    let else_target = self.points.get_next_point();
                    else_stmt
                        .get()
                        .walk_mut(self, else_stmt.loc(), else_stmt.id());
                    self.points.set_branch_target(else_branch, else_target);
                    // Fall-through to end.

                    let end_target = self.points.get_next_point();
                    self.points.set_branch_target(end_branch, end_target);
                } else {
                    // Branch to then block.
                    let then_branch = self.points.add_goto_if(cond_expr.id(), cond_expr.get());
                    // If not branch to end block.
                    let end_branch = self.points.add_goto();

                    // Execute then block.
                    let then_target = self.points.get_next_point();
                    then_stmt
                        .get()
                        .walk_mut(self, then_stmt.loc(), then_stmt.id());
                    self.points.set_branch_target(then_branch, then_target);
                    // Fall-through to end.

                    let end_target = self.points.get_next_point();
                    self.points.set_branch_target(end_branch, end_target);
                }
                return false;
            }
            &ast::Stmt::WhileDo(stmt_while) => {
                let cond_expr = &stmt_while.0;
                let body_stmt = &stmt_while.1;

                let cond_target = self.points.get_next_point();
                self.points.add_expr(cond_expr.id(), cond_expr.get());
                let loop_body_branch = self.points.add_goto_if(cond_expr.id(), cond_expr.get());
                let end_loop_branch = self.points.add_goto();

                let loop_body_target = self.points.get_next_point();
                body_stmt
                    .get()
                    .walk_mut(self, body_stmt.loc(), body_stmt.id());

                self.points
                    .set_branch_target(loop_body_branch, loop_body_target);

                let cond_branch = self.points.add_goto();
                self.points.set_branch_target(cond_branch, cond_target);

                let end_loop_target = self.points.get_next_point();
                self.points
                    .set_branch_target(end_loop_branch, end_loop_target);

                return false;
            }
            _ => {
                return true;
            }
        }
    }

    fn visit_post_stmt(
        &mut self,
        n: &'a ast::Stmt,
        _span: &pasko_frontend::span::SpanLoc,
        id: pasko_frontend::span::SpanId,
    ) {
        self.points.add_stmt(id, n);
    }
}

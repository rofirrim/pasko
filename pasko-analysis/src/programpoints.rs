use std::collections::HashMap;
use std::num::NonZero;

use crate::visitor;
use crate::visitor::Visitable;

use pasko_frontend::ast::{self, Program};
use pasko_frontend::span::{SpanId, SpannedBox};

type ProgramCounter = usize;

pub struct ProgramPoints<'a> {
    // Sequence of AST trees expressing the order of the program.
    // Some of those trees will contain branches, so we need to map them again.
    program_points: Vec<ProgramPoint<'a>>,
}

pub enum ProgramPoint<'a> {
    Stmt(SpanId, &'a ast::Stmt),
    Expr(SpanId, &'a ast::Expr),
    BranchIf(SpanId, &'a ast::Expr, ProgramCounter),
    Branch(ProgramCounter),
    NoOperation, // Can we remove this?
}

impl<'a> ProgramPoints<'a> {
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

        self.program_points.len()
    }

    // Adds a conditional goto to fixup later
    #[must_use]
    fn add_goto_if(&mut self, id: SpanId, expr: &'a ast::Expr) -> usize {
        self.program_points
            .push(ProgramPoint::BranchIf(id, expr, 0));

        self.program_points.len()
    }

    fn set_branch_target(&mut self, pc: ProgramCounter, target_pc: ProgramCounter) {
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

    // This always inserts a nop
    fn set_branch_target_to_current_point(&mut self, pc: ProgramCounter) {
        self.program_points.push(ProgramPoint::NoOperation);
        self.set_branch_target(pc, self.program_points.len() - 1);
    }
}

pub struct ProgramPointsBuilder<'a> {
    points: ProgramPoints<'a>,
}

impl<'a> ProgramPointsBuilder<'a> {
    pub fn new_program(p: &'a ast::ProgramBlock) -> ProgramPoints<'a> {
        todo!()
    }

    pub fn new_function(p: &'a ast::FunctionDefinition) -> ProgramPoints<'a> {
        todo!()
    }

    pub fn new_procedure(p: &'a ast::ProcedureDefinition) -> ProgramPoints<'a> {
        todo!()
    }
}

impl<'a> visitor::VisitorMut<'a> for ProgramPointsBuilder<'a> {
    fn visit_pre_expr(
        &mut self,
        n: &'a ast::Expr,
        _span: &pasko_frontend::span::SpanLoc,
        id: pasko_frontend::span::SpanId,
    ) -> bool {
        self.points.add_expr(id, n);
        false
    }

    fn visit_post_stmt(
        &mut self,
        n: &'a ast::Stmt,
        _span: &pasko_frontend::span::SpanLoc,
        id: pasko_frontend::span::SpanId,
    ) {
        match &n {
            &ast::Stmt::If(stmt_if) => {
                // Foo
                let cond_expr = &stmt_if.0;
                let then_stmt = &stmt_if.1;
                let else_stmt = &stmt_if.2;
                self.points.add_expr(cond_expr.id(), cond_expr.get());
                if let Some(else_stmt) = else_stmt {
                } else {
                    let true_branch = self.points.add_goto_if(cond_expr.id(), cond_expr.get());
                    let end_branch = self.points.add_goto();
                    self.points.set_branch_target_to_current_point(true_branch);
                    then_stmt
                        .get()
                        .walk_mut(self, then_stmt.loc(), then_stmt.id());
                    self.points.set_branch_target_to_current_point(end_branch);
                }
            }
            _ => {
                self.points.add_stmt(id, n);
            }
        }
    }
}

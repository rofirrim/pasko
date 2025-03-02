use std::collections::HashMap;
use std::num::NonZero;

use crate::visitor;

use pasko_frontend::ast::{self, Program};
use pasko_frontend::span::{SpanId, SpannedBox};

type ProgramCounter = usize;

pub struct ProgramPoints<'a> {
    // Sequence of AST trees expressing the order of the program.
    // Some of those trees will contain branches, so we need to map them again.
    program_points: Vec<ProgramPoint<'a>>,
}

pub enum ProgramPoint<'a> {
    Stmt(&'a ast::Stmt),
    Expr(&'a ast::Expr),
    BranchIf(&'a ast::Expr, ProgramCounter),
    Branch(ProgramCounter),
}

impl<'a> ProgramPoints<'a> {
    fn add_expr(&mut self, expr: &'a ast::Expr) {
        self.program_points.push(ProgramPoint::Expr(expr))
    }
    fn add_stmt(&mut self, stmt: &'a ast::Stmt) {
        self.program_points.push(ProgramPoint::Stmt(stmt))
    }

    // Adds a goto to fixup later
    #[must_use]
    fn add_goto(&mut self) -> usize {
        self.program_points.push(ProgramPoint::Branch(0));

        self.program_points.len()
    }

    // Adds a conditional goto to fixup later
    #[must_use]
    fn add_goto_if(&mut self, expr: &'a ast::Expr) -> usize {
        self.program_points.push(ProgramPoint::BranchIf(expr, 0));

        self.program_points.len()
    }

    fn set_branch_target(&mut self, pc: ProgramCounter, target_pc: ProgramCounter) {
        match &mut self.program_points[pc] {
            ProgramPoint::Branch(x) => {
                *x = target_pc;
            }
            ProgramPoint::BranchIf(_, x) => {
                *x = target_pc;
            }
            _ => {
                panic!("Instruction is not a branch");
            }
        }
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
        _id: pasko_frontend::span::SpanId,
    ) -> bool {
        self.points.add_expr(n);
        false
    }
}

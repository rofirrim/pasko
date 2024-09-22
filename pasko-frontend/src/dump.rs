use crate::ast;
use crate::semantic::SemanticContext;
use crate::span;
use crate::visitor::{Visitable, VisitorMut};
use std::fmt::Display;

pub struct ASTDumper<'a> {
    prefix: String,
    output: String,
    linemap: span::LineMap,
    semantic_context: &'a SemanticContext,
    no_ids: bool,
}

impl<'a> ASTDumper<'a> {
    pub fn new(input: &str, semantic_context: &'a SemanticContext) -> ASTDumper<'a> {
        ASTDumper {
            prefix: String::new(),
            output: String::new(),
            linemap: span::LineMap::new(input),
            semantic_context,
            no_ids: false,
        }
    }

    pub fn set_no_ids(&mut self) {
        self.no_ids = true;
    }

    fn emit_line(&mut self, classname: &str, span: &span::SpanLoc, id: span::SpanId) {
        self.emit_line_impl(classname, span, id, "");
    }

    fn emit_line_payload(
        &mut self,
        classname: &str,
        span: &span::SpanLoc,
        id: span::SpanId,
        payload: &str,
    ) {
        let payload = format!("{}", payload);
        self.emit_line_impl(classname, span, id, &payload);
    }

    fn emit_line_impl(
        &mut self,
        classname: &str,
        span: &span::SpanLoc,
        id: span::SpanId,
        payload: &str,
    ) {
        let line = if self.no_ids {
            format!("{} {} {}", classname, self.get_loc(span), payload)
        } else {
            format!(
                "{}[{}] {} {}",
                classname,
                id.get_number(),
                self.get_loc(span),
                payload
            )
        };
        self.output.push_str(self.get_prefix().as_str());
        self.output.push_str(&line);
        self.output.push('\n');
    }

    fn prefix_push_child(&mut self) {
        self.prefix.push('├');
    }

    fn prefix_push_last_child(&mut self) {
        self.prefix.push('└');
    }

    fn prefix_pop(&mut self) {
        self.prefix.pop();
    }

    fn get_prefix(&self) -> String {
        let mut res = String::new();

        let char_len = self.prefix.chars().count();

        for (idx, c) in self.prefix.chars().enumerate() {
            if idx + 1 == char_len {
                res.push(c);
                res.push('─');
                res.push('╴');
            } else if c == '├' {
                res.push('│');
                res.push(' ');
                res.push(' ');
            } else if c == '└' {
                res.push(' ');
                res.push(' ');
                res.push(' ');
            }
        }

        res
    }

    fn walk_child<T: Visitable>(&mut self, n: &span::SpannedBox<T>) {
        self.prefix_push_child();
        n.get().walk_mut(self, n.loc(), n.id());
        self.prefix_pop();
    }

    fn walk_optional_child<T: Visitable>(&mut self, n: &Option<span::SpannedBox<T>>) {
        if let Some(n) = n {
            self.walk_child(n);
        }
    }

    fn walk_last_child<T: Visitable>(&mut self, n: &span::SpannedBox<T>) {
        self.prefix_push_last_child();
        n.get().walk_mut(self, n.loc(), n.id());
        self.prefix_pop();
    }

    fn walk_vec_child<T: Visitable>(&mut self, n: &Vec<span::SpannedBox<T>>) {
        for (idx, c) in n.iter().enumerate() {
            if idx == n.len() - 1 {
                self.walk_last_child(&c);
            } else {
                self.walk_child(&c);
            }
        }
    }

    fn walk_vec_child_not_last<T: Visitable>(&mut self, n: &Vec<span::SpannedBox<T>>) {
        for c in n.iter() {
            self.walk_child(&c);
        }
    }

    fn walk_optional_vec_child<T: Visitable>(&mut self, n: &Option<Vec<span::SpannedBox<T>>>) {
        if let Some(n) = n {
            for (idx, c) in n.iter().enumerate() {
                if idx == n.len() - 1 {
                    self.walk_last_child(&c);
                } else {
                    self.walk_child(&c);
                }
            }
        }
    }

    fn get_loc(&self, span: &span::SpanLoc) -> String {
        let (line, col) = self.linemap.offset_to_line_and_col(span.begin());
        format!("{}:{}", line, col)
    }

    fn type_to_string(&self, id: span::SpanId) -> String {
        let t = self.semantic_context.get_ast_type(id);
        if let Some(type_id) = t {
            if self.semantic_context.type_system.is_error_type(type_id) {
                return String::from("<<error-type>>");
            }
            self.semantic_context.type_system.get_type_name(type_id)
        } else {
            String::from("<<no-type>>")
        }
    }
}

impl<'a> Display for ASTDumper<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.output)
    }
}

impl<'a> VisitorMut for ASTDumper<'a> {
    fn unhandled_node_pre(&self, class: &str, span: &span::SpanLoc, _id: span::SpanId) {
        panic!("Unhandled node |{}| at {}", class, self.get_loc(span));
    }

    fn unhandled_node_leaf(&self, class: &str, span: &span::SpanLoc, _id: span::SpanId) {
        panic!("Unhandled leaf node |{}| at {}", class, self.get_loc(span));
    }

    fn visit_pre_program(
        &mut self,
        n: &ast::Program,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("Program", span, id);

        self.walk_child(&n.0);
        self.walk_last_child(&n.1);

        false
    }

    fn visit_program_heading(
        &mut self,
        n: &ast::ProgramHeading,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let payload = format!(
            "{:?} {:?}",
            n.0.get(),
            n.1.iter().map(|x| x.get()).collect::<Vec<&String>>()
        );
        self.emit_line_payload("ProgramHeading", span, id, &payload);
    }

    fn visit_pre_program_block(
        &mut self,
        n: &ast::ProgramBlock,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("ProgramBlock", span, id);

        self.walk_last_child(&n.0);

        false
    }

    fn visit_pre_block(&mut self, n: &ast::Block, span: &span::SpanLoc, id: span::SpanId) -> bool {
        self.emit_line("Block", span, id);

        self.walk_optional_child(&n.0);
        self.walk_optional_child(&n.1);
        self.walk_optional_child(&n.2);
        self.walk_optional_child(&n.3);
        self.walk_optional_child(&n.4);
        self.walk_last_child(&n.5);

        false
    }

    fn visit_pre_statement_part(
        &mut self,
        n: &ast::StatementPart,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("Statement", span, id);

        self.walk_last_child(&n.0);

        false
    }

    fn visit_pre_stmt_case(
        &mut self,
        n: &ast::StmtCase,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("StmtCase", span, id);

        self.walk_child(&n.0);
        self.walk_vec_child(&n.1);

        false
    }

    fn visit_pre_case_list_element(
        &mut self,
        n: &ast::CaseListElement,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("CaseListElement", span, id);

        n.0.iter().for_each(|x| self.walk_child(x));
        self.walk_last_child(&n.1);

        false
    }

    fn visit_pre_stmt_compound(
        &mut self,
        n: &ast::StmtCompound,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("StmtCompound", span, id);

        self.walk_vec_child(&n.0);
        false
    }

    fn visit_pre_stmt_procedure_call(
        &mut self,
        n: &ast::StmtProcedureCall,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload("StmtProcedureCall", span, id, &format!("{:?}", n.0.get()));

        self.walk_optional_vec_child(&n.1);
        false
    }

    fn visit_stmt_empty(&mut self, _n: &ast::StmtEmpty, span: &span::SpanLoc, id: span::SpanId) {
        self.emit_line("StmtEmpty", span, id);
    }

    fn visit_pre_stmt_for(
        &mut self,
        n: &ast::StmtFor,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "StmtFor",
            span,
            id,
            &format!(
                "{}",
                match n.0 {
                    ast::ForKind::To => "to",
                    ast::ForKind::DownTo => "downto",
                }
            ),
        );

        self.walk_child(&n.1);
        self.walk_child(&n.2);
        self.walk_child(&n.3);
        self.walk_last_child(&n.4);
        false
    }

    fn visit_pre_stmt_while_do(
        &mut self,
        n: &ast::StmtWhileDo,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("StmtWhileDo", span, id);

        self.walk_child(&n.0);
        self.walk_last_child(&n.1);
        false
    }

    fn visit_pre_stmt_if(
        &mut self,
        n: &ast::StmtIf,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("StmtIf", span, id);

        self.walk_child(&n.0);
        if n.2.is_some() {
            self.walk_child(&n.1);
            self.walk_last_child(n.2.as_ref().unwrap());
        } else {
            self.walk_last_child(&n.1);
        }
        false
    }

    fn visit_pre_stmt_repeat_until(
        &mut self,
        n: &ast::StmtRepeatUntil,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("StmtRepeatUntil", span, id);

        n.0.iter().for_each(|x| self.walk_child(x));
        self.walk_last_child(&n.1);

        false
    }

    fn visit_pre_expr_function_call(
        &mut self,
        n: &ast::ExprFunctionCall,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "ExprFunctionCall",
            span,
            id,
            &format!("{} {}", n.0.get(), self.type_to_string(id)),
        );

        self.walk_vec_child(&n.1);

        false
    }

    fn visit_pre_expr_const(
        &mut self,
        n: &ast::ExprConst,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "ExprConst",
            span,
            id,
            &format!(
                "{:?} {}",
                self.type_to_string(id),
                self.semantic_context
                    .get_ast_value(id)
                    .map_or("<<no const>>".to_string(), |x| x.to_string())
            ),
        );

        self.walk_last_child(&n.0);

        false
    }

    fn visit_const_string_literal(
        &mut self,
        n: &ast::ConstStringLiteral,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        self.emit_line_payload("ConstStringLiteral", span, id, &format!("{:?}", n.0.get()));
    }

    fn visit_const_nil(&mut self, _n: &ast::ConstNil, span: &span::SpanLoc, id: span::SpanId) {
        self.emit_line("ConstNil", span, id);
    }

    fn visit_pre_expr_variable(
        &mut self,
        n: &ast::ExprVariable,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "ExprVariable",
            span,
            id,
            &format!("{}", self.type_to_string(id)),
        );

        self.walk_last_child(&n.0);

        false
    }

    fn visit_pre_expr_variable_reference(
        &mut self,
        n: &ast::ExprVariableReference,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "ExprVariableReference",
            span,
            id,
            &format!("{}", self.type_to_string(id)),
        );

        self.walk_last_child(&n.0);

        false
    }

    fn visit_assig_variable(
        &mut self,
        n: &ast::AssigVariable,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        self.emit_line_payload(
            "AssigVariable",
            span,
            id,
            &format!("{:?} {}", n.0.get(), self.type_to_string(id)),
        );
    }

    fn visit_pre_assig_array_access(
        &mut self,
        n: &ast::AssigArrayAccess,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "AssigArrayAccess",
            span,
            id,
            &format!("{}", self.type_to_string(id)),
        );

        self.walk_child(&n.0);

        self.walk_vec_child(&n.1);

        false
    }

    fn visit_pre_assig_field_access(
        &mut self,
        n: &ast::AssigFieldAccess,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "AssigFieldAccess",
            span,
            id,
            &format!("field:<{}> {}", n.1.get(), self.type_to_string(id)),
        );

        self.walk_last_child(&n.0);

        false
    }

    fn visit_pre_assig_pointer_deref(
        &mut self,
        n: &ast::AssigPointerDeref,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "AssigPointerDeref",
            span,
            id,
            &format!("{}", self.type_to_string(id)),
        );

        self.walk_last_child(&n.0);

        false
    }

    fn visit_pre_stmt_assignment(
        &mut self,
        n: &ast::StmtAssignment,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "StmtAssignment",
            span,
            id,
            &format!(
                "lhs {} rhs {}",
                self.type_to_string(n.0.id()),
                self.type_to_string(n.1.id())
            ),
        );
        self.walk_child(&n.0);
        self.walk_last_child(&n.1);
        false
    }

    fn visit_const_integer(
        &mut self,
        n: &ast::ConstInteger,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        self.emit_line_payload(
            "ConstInteger",
            span,
            id,
            &format!("{:?} {}", n.0.get(), self.type_to_string(id)),
        );
    }

    fn visit_const_bool(&mut self, n: &ast::ConstBool, span: &span::SpanLoc, id: span::SpanId) {
        self.emit_line_payload(
            "ConstBool",
            span,
            id,
            &format!("{:?} {}", n.0.get(), self.type_to_string(id)),
        );
    }

    fn visit_const_real(&mut self, n: &ast::ConstReal, span: &span::SpanLoc, id: span::SpanId) {
        self.emit_line_payload(
            "ConstReal",
            span,
            id,
            &format!("{:?} {}", n.0.get(), self.type_to_string(id)),
        );
    }

    fn visit_const_named(&mut self, n: &ast::ConstNamed, span: &span::SpanLoc, id: span::SpanId) {
        self.emit_line_payload(
            "ConstNamed",
            span,
            id,
            &format!(
                "{:?} {} {}",
                n.0.get(),
                self.type_to_string(id),
                self.semantic_context
                    .get_ast_value(id)
                    .map_or("<<no const>>".to_string(), |x| x.to_string())
            ),
        );
    }

    fn visit_const_minus_named(
        &mut self,
        n: &ast::ConstMinusNamed,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        self.emit_line_payload(
            "ConstMinusNamed",
            span,
            id,
            &format!(
                "{:?} {} {}",
                n.0.get(),
                self.type_to_string(id),
                self.semantic_context
                    .get_ast_value(id)
                    .map_or("<<no const>>".to_string(), |x| x.to_string())
            ),
        );
    }

    fn visit_pre_expr_bin_op(
        &mut self,
        n: &ast::ExprBinOp,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "BinOp",
            span,
            id,
            &format!("{} {}", n.0.get(), self.type_to_string(id)),
        );

        self.walk_child(&n.1);
        self.walk_last_child(&n.2);

        false
    }

    fn visit_pre_expr_un_op(
        &mut self,
        n: &ast::ExprUnOp,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "UnOp",
            span,
            id,
            &format!("{} {}", n.0.get(), self.type_to_string(id)),
        );

        self.walk_last_child(&n.1);

        false
    }

    fn visit_pre_expr_parentheses(
        &mut self,
        n: &ast::ExprParentheses,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("ExprParentheses", span, id);

        self.walk_last_child(&n.0);

        false
    }

    fn visit_pre_expr_conversion(
        &mut self,
        n: &ast::ExprConversion,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "Conversion",
            span,
            id,
            &format!("{}", self.type_to_string(id)),
        );

        self.walk_last_child(&n.0);

        false
    }

    fn visit_expr_bound_identifier(
        &mut self,
        n: &ast::ExprBoundIdentifier,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        self.emit_line_payload(
            "BoundIdentifier",
            span,
            id,
            &format!("{} {}", n.0.get(), self.type_to_string(id)),
        );
    }

    fn visit_pre_variable_declaration_part(
        &mut self,
        n: &ast::VariableDeclarationPart,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("VariableDeclarationPart", span, id);

        self.walk_vec_child(&n.0);

        false
    }

    fn visit_pre_variable_declaration(
        &mut self,
        n: &ast::VariableDeclaration,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "VariableDeclaration",
            span,
            id,
            &format!(
                "{:?}",
                n.0.iter().map(|x| x.get()).collect::<Vec<&String>>()
            ),
        );

        self.walk_last_child(&n.1);

        false
    }

    fn visit_type_identifier(
        &mut self,
        n: &ast::TypeIdentifier,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        self.emit_line_payload(
            "TypeIdentifier",
            span,
            id,
            &format!("{:?} {}", n.0.get(), self.type_to_string(id)),
        );
    }

    fn visit_pre_array_type(
        &mut self,
        n: &ast::ArrayType,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "ArrayType",
            span,
            id,
            &format!(
                "{}",
                if n.0.is_some() {
                    n.0.as_ref().unwrap().get().clone()
                } else {
                    "(unpacked)".to_string()
                }
            ),
        );

        n.1.iter().for_each(|x| {
            self.walk_child(x);
        });
        self.walk_last_child(&n.2);

        false
    }

    fn visit_pre_subrange_type(
        &mut self,
        n: &ast::SubrangeType,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("SubRangeType", span, id);

        self.walk_child(&n.0);
        self.walk_last_child(&n.1);

        false
    }

    fn visit_enumerated_type(
        &mut self,
        n: &ast::EnumeratedType,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        self.emit_line_payload(
            "EnumeratedType",
            span,
            id,
            &format!(
                "{}",
                n.0.iter()
                    .map(|x| x.get())
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        );
    }

    fn visit_pre_file_type(
        &mut self,
        n: &ast::FileType,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "FileType",
            span,
            id,
            &format!(
                "{}",
                if n.0.is_some() {
                    n.0.as_ref().unwrap().get().clone()
                } else {
                    "(unpacked)".to_string()
                }
            ),
        );

        self.walk_last_child(&n.1);

        false
    }

    fn visit_pre_pointer_type(
        &mut self,
        n: &ast::PointerType,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("PointerType", span, id);

        self.walk_last_child(&n.0);

        false
    }

    fn visit_pre_record_type(
        &mut self,
        n: &ast::RecordType,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "RecordType",
            span,
            id,
            &format!(
                "{}",
                if n.0.is_some() {
                    n.0.as_ref().unwrap().get().clone()
                } else {
                    "(unpacked)".to_string()
                }
            ),
        );

        self.walk_last_child(&n.1);

        false
    }

    fn visit_pre_field_list(
        &mut self,
        n: &ast::FieldList,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("FieldList", span, id);

        match (&n.0, &n.1) {
            (Some(fixed_part), Some(variant_part)) => {
                self.walk_vec_child_not_last(fixed_part);
                self.walk_last_child(variant_part);
            }
            (None, Some(variant_part)) => {
                self.walk_last_child(variant_part);
            }
            (Some(fixed_part), None) => {
                self.walk_vec_child(fixed_part);
            }
            (None, None) => {}
        }

        false
    }

    fn visit_pre_variant_part(
        &mut self,
        n: &ast::VariantPart,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("VariantPart", span, id);

        self.walk_child(&n.0);
        self.walk_vec_child(&n.1);

        false
    }

    fn visit_pre_variant(
        &mut self,
        n: &ast::Variant,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("Variant", span, id);

        self.walk_vec_child_not_last(&n.0);
        self.walk_last_child(&n.1);

        false
    }

    fn visit_pre_variant_selector(
        &mut self,
        n: &ast::VariantSelector,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "VariantSelector",
            span,
            id,
            &format!(
                "{}",
                if n.0.is_some() {
                    n.0.as_ref().unwrap().get()
                } else {
                    "[unnamed]"
                },
            ),
        );

        self.walk_last_child(&n.1);

        false
    }

    fn visit_pre_record_section(
        &mut self,
        n: &ast::RecordSection,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "RecordSection",
            span,
            id,
            &format!(
                "{}",
                n.0.iter()
                    .map(|x| x.get().clone())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        );

        self.walk_last_child(&n.1);

        false
    }

    fn visit_pre_set_type(
        &mut self,
        n: &ast::SetType,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "SetType",
            span,
            id,
            &format!(
                "{}",
                if n.0.is_some() {
                    n.0.as_ref().unwrap().get().clone()
                } else {
                    "(unpacked)".to_string()
                }
            ),
        );

        self.walk_last_child(&n.1);

        false
    }

    fn visit_pre_expr_range(
        &mut self,
        n: &ast::ExprRange,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "ExprRange",
            span,
            id,
            &format!("{}", self.type_to_string(id)),
        );

        self.walk_child(&n.0);
        self.walk_last_child(&n.1);

        false
    }

    fn visit_pre_expr_set_literal(
        &mut self,
        n: &ast::ExprSetLiteral,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "ExprSetLiteral",
            span,
            id,
            &format!("{}", self.type_to_string(id)),
        );

        self.walk_vec_child(&n.0);

        false
    }

    fn visit_pre_constant_definition_part(
        &mut self,
        n: &ast::ConstantDefinitionPart,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("ConstantDefinitionPart", span, id);

        self.walk_vec_child(&n.0);

        false
    }

    fn visit_pre_constant_definition(
        &mut self,
        n: &ast::ConstantDefinition,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload("ConstantDefinition", span, id, &format!("{}", n.0.get()));

        self.walk_last_child(&n.1);
        false
    }

    fn visit_pre_type_denoter(
        &mut self,

        _n: &ast::TypeDenoter,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        true
    }

    fn visit_pre_function_forward(
        &mut self,
        n: &ast::FunctionForward,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload("FunctionForward", span, id, &format!("{}", n.0.get()));

        if let Some(x) = &n.1 {
            x.iter().for_each(|x| self.walk_child(x));
        }
        self.walk_last_child(&n.2);

        false
    }

    fn visit_pre_function_definition(
        &mut self,
        n: &ast::FunctionDefinition,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload("FunctionDefinition", span, id, &format!("{}", n.0.get()));

        if let Some(x) = &n.1 {
            x.iter().for_each(|x| self.walk_child(x));
        }

        self.walk_child(&n.2);
        self.walk_last_child(&n.3);

        false
    }

    fn visit_pre_function_late_definition(
        &mut self,
        n: &ast::FunctionLateDefinition,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "FunctionLateDefinition",
            span,
            id,
            &format!("{}", n.0.get()),
        );

        self.walk_last_child(&n.1);

        false
    }

    fn visit_pre_procedure(
        &mut self,
        n: &ast::Procedure,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("Procedure", span, id);

        self.walk_last_child(&n.0);

        false
    }

    fn visit_pre_function(
        &mut self,
        n: &ast::Function,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("Function", span, id);

        self.walk_last_child(&n.0);

        false
    }

    fn visit_pre_procedure_definition(
        &mut self,
        n: &ast::ProcedureDefinition,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload("ProcedureDefinition", span, id, &format!("{}", n.0.get()));

        if let Some(x) = &n.1 {
            x.iter().for_each(|x| self.walk_child(x));
        }

        self.walk_last_child(&n.2);

        false
    }

    fn visit_pre_procedure_forward(
        &mut self,
        n: &ast::ProcedureForward,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload("ProcedureForward", span, id, &format!("{}", n.0.get()));

        self.walk_optional_vec_child(&n.1);

        false
    }

    fn visit_pre_procedure_and_function_declaration_part(
        &mut self,
        n: &ast::ProcedureAndFunctionDeclarationPart,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("ProcedureAndFunctionDeclarationPart", span, id);

        self.walk_vec_child(&n.0);

        false
    }

    fn visit_pre_formal_parameter_value(
        &mut self,
        n: &ast::FormalParameterValue,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "FormalParameterValue",
            span,
            id,
            &format!(
                "{:?}",
                n.0.iter().map(|x| x.get()).collect::<Vec<&String>>()
            ),
        );
        self.walk_last_child(&n.1);

        false
    }

    fn visit_pre_formal_parameter_variable(
        &mut self,
        n: &ast::FormalParameterVariable,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "FormalParameterVariable",
            span,
            id,
            &format!(
                "{:?}",
                n.0.iter().map(|x| x.get()).collect::<Vec<&String>>()
            ),
        );
        self.walk_last_child(&n.1);

        false
    }

    fn visit_pre_formal_parameter_function(
        &mut self,
        n: &ast::FormalParameterFunction,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "FormalParameterFunction",
            span,
            id,
            &format!("{}", n.0.get()),
        );

        if let Some(v) = &n.1 {
            v.iter().for_each(|x| self.walk_child(x));
        }

        self.walk_last_child(&n.2);

        false
    }

    fn visit_pre_formal_parameter_procedure(
        &mut self,
        n: &ast::FormalParameterProcedure,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "FormalParameterProcedure",
            span,
            id,
            &format!("{}", n.0.get()),
        );

        if let Some(v) = &n.1 {
            v.iter().for_each(|x| self.walk_child(x));
        }

        false
    }

    fn visit_pre_formal_parameter_value_conformable_array(
        &mut self,
        n: &ast::FormalParameterValueConformableArray,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "FormalParameterValueConformableArray",
            span,
            id,
            &format!(
                "{:?}",
                n.0.iter().map(|x| x.get()).collect::<Vec<&String>>()
            ),
        );

        self.walk_last_child(&n.1);

        false
    }

    fn visit_pre_formal_parameter_variable_conformable_array(
        &mut self,
        n: &ast::FormalParameterVariableConformableArray,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "FormalParamVariablealueConformableArray",
            span,
            id,
            &format!(
                "{:?}",
                n.0.iter().map(|x| x.get()).collect::<Vec<&String>>()
            ),
        );

        self.walk_last_child(&n.1);

        false
    }

    fn visit_pre_conformable_array_schema(
        &mut self,
        n: &ast::ConformableArraySchema,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "ConformableArraySchema",
            span,
            id,
            &format!(
                "{}",
                if n.0.is_some() {
                    n.0.as_ref().unwrap()
                } else {
                    ""
                }
            ),
        );

        self.walk_vec_child_not_last(&n.1);
        self.walk_last_child(&n.2);

        false
    }

    fn visit_pre_array_schema(
        &mut self,
        n: &ast::ArraySchema,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("ArraySchema", span, id);

        self.walk_last_child(&n.0);

        false
    }

    fn visit_pre_index_type_specification(
        &mut self,
        n: &ast::IndexTypeSpecification,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload(
            "IndexTypeSpecification",
            span,
            id,
            &format!("{}..{}", n.0.get(), n.1.get()),
        );

        self.walk_last_child(&n.2);

        false
    }

    fn visit_pre_type_definition_part(
        &mut self,
        n: &ast::TypeDefinitionPart,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line("TypeDefinitionPart", span, id);

        self.walk_vec_child(&n.0);

        false
    }

    fn visit_pre_type_definition(
        &mut self,
        n: &ast::TypeDefinition,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.emit_line_payload("TypeDefinition", span, id, &format!("{}", n.0.get()));

        self.walk_last_child(&n.1);

        false
    }

    // Generic visitors of enumerators. They must return true so their variants are visited.
    fn visit_pre_procedure_and_function_declaration(
        &mut self,
        _n: &ast::ProcedureAndFunctionDeclaration,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        true
    }
    fn visit_pre_procedure_declaration(
        &mut self,
        _n: &ast::ProcedureDeclaration,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        true
    }
    fn visit_pre_function_declaration(
        &mut self,
        _n: &ast::FunctionDeclaration,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        true
    }
    fn visit_pre_formal_parameter(
        &mut self,
        _n: &ast::FormalParameter,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        true
    }
    fn visit_pre_stmt(&mut self, _n: &ast::Stmt, _span: &span::SpanLoc, _id: span::SpanId) -> bool {
        true
    }
    fn visit_pre_expr(&mut self, _n: &ast::Expr, _span: &span::SpanLoc, _id: span::SpanId) -> bool {
        true
    }
    fn visit_pre_const(
        &mut self,
        _n: &ast::Const,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        true
    }
    fn visit_pre_assig(
        &mut self,
        _n: &ast::Assig,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        true
    }

    fn visit_pre_conformable_array_element(
        &mut self,
        _n: &ast::ConformableArrayElement,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        true
    }
}

use crate::ast::{self, ExprVariableReference, UnaryOp};
use crate::ast::{BinOperand, FormalParameter};
use crate::constant::Constant;
use crate::diagnostics::{Diagnostic, DiagnosticKind, Diagnostics};
use crate::scope;
use crate::span;
use crate::symbol::{
    ParameterKind, Symbol, SymbolId, SymbolKind, SymbolMap, SymbolMapImpl, SymbolRef,
};
use crate::typesystem::{Type, TypeId, TypeKind, TypeSystem};
use crate::visitor::MutatingVisitable;
use crate::visitor::MutatingVisitorMut;

use std::collections::HashMap;

pub struct SemanticContext {
    symbol_map: SymbolMap,
    pub type_system: TypeSystem,

    ast_types: HashMap<span::SpanId, TypeId>,
    ast_symbols: HashMap<span::SpanId, SymbolId>,
    ast_values: HashMap<span::SpanId, Constant>,
}

const REQUIRED_PROCEDURES: &[&str] = &[
    "rewrite", "put", "reset", "get", "read", "write", "new", "dispose", "pack", "unpack",
    "writeln", "readln", "page",
];
const REQUIRED_FUNCTIONS: &[&str] = &[
    "abs", "sqr", "sin", "cos", "exp", "ln", "sqrt", "arctan", "trunc", "round", "ord", "chr",
    "succ", "pred", "odd", "eof", "eoln",
];

pub fn is_required_procedure(name: &str) -> bool {
    REQUIRED_PROCEDURES.iter().find(|x| name == **x).is_some()
}

pub fn is_required_function(name: &str) -> bool {
    REQUIRED_FUNCTIONS.iter().find(|x| name == **x).is_some()
}

pub fn is_required_procedure_or_function(name: &str) -> bool {
    is_required_procedure(name) || is_required_function(name)
}

// In bytes.
impl SemanticContext {
    pub fn new() -> SemanticContext {
        let symbol_map = SymbolMapImpl::new();
        let type_system = TypeSystem::new(symbol_map.clone());

        let sc = SemanticContext {
            symbol_map,
            type_system,

            ast_types: HashMap::new(),
            ast_symbols: HashMap::new(),
            ast_values: HashMap::new(),
        };

        sc
    }

    pub fn get_ast_type(&self, id: span::SpanId) -> Option<TypeId> {
        self.ast_types.get(&id).cloned()
    }

    pub fn set_ast_type(&mut self, id: span::SpanId, ty: TypeId) {
        self.ast_types.insert(id, ty);
    }

    pub fn get_ast_value(&self, id: span::SpanId) -> Option<Constant> {
        self.ast_values.get(&id).cloned()
    }

    pub fn set_ast_value(&mut self, id: span::SpanId, ty: Constant) {
        self.ast_values.insert(id, ty);
    }

    pub fn new_symbol(&mut self, sym: Symbol) -> SymbolId {
        self.symbol_map.borrow_mut().new_symbol(sym)
    }

    pub fn get_symbol(&self, id: SymbolId) -> SymbolRef {
        self.symbol_map.borrow().get_symbol(id)
    }

    pub fn get_symbol_mut(&mut self, id: SymbolId) -> SymbolRef {
        self.symbol_map.borrow_mut().get_symbol_mut(id)
    }

    pub fn set_ast_symbol(&mut self, id: span::SpanId, sym: SymbolId) {
        self.ast_symbols.insert(id, sym);
    }

    pub fn get_ast_symbol(&self, id: span::SpanId) -> Option<SymbolId> {
        self.ast_symbols.get(&id).cloned()
    }
}

struct SemanticCheckerVisitor<'a> {
    ctx: &'a mut SemanticContext,
    diagnostics: &'a mut Diagnostics,
    scope: &'a mut scope::Scope,
}

enum FunctionProcedureDeclarationStatus {
    /// The identifier has not been declared.
    NotDeclared,
    /// The identifier has been forward declared as a compatible symbol kind and
    /// should be checked for compatibility against its definition.
    ForwardDeclared,
    /// Already declared as something else. This is an error.
    AlreadyDeclared,
    /// Already defined. This is an error.
    AlreadyDefined,
}

impl<'a> SemanticCheckerVisitor<'a> {
    fn lookup_symbol(&mut self, name: &str, span: &span::SpanLoc) -> Option<SymbolId> {
        let query = self.scope.lookup(name);
        if query.is_none() {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *span,
                format!("identifier '{}' not found in this scope", name),
            );
            // Now create an erroneous identifier that we will use to filter later occurrences.
            let mut dummy_sym = Symbol::new();
            dummy_sym.set_name(name);
            dummy_sym.set_kind(SymbolKind::ErrorLookup);
            dummy_sym.set_defining_point(*span);

            let dummy_sym = self.ctx.new_symbol(dummy_sym);

            self.scope.add_entry(name, dummy_sym);
        }
        query
    }

    fn extra_diag_previous_location(&self, symbol: &Symbol) -> Vec<Diagnostic> {
        let mut extra = vec![];

        if symbol.get_defining_point().is_some() {
            extra.push(Diagnostic::new(
                DiagnosticKind::Info,
                symbol.get_defining_point().unwrap(),
                format!(
                    "location of previous declaration{}",
                    if let Some(kind_symbol) = symbol.get_name_of_kind() {
                        format!(", declared as a {}", kind_symbol)
                    } else {
                        "".to_string()
                    }
                ),
            ));
        }

        extra
    }

    fn diagnose_redeclared_symbol(&mut self, name: &String, span: &span::SpanLoc) -> bool {
        let sym = self.scope.lookup_current_scope(name);
        match sym {
            Some(x) => {
                let extra = self.extra_diag_previous_location(&self.ctx.get_symbol(x).borrow());
                self.diagnostics.add_with_extra(
                    DiagnosticKind::Error,
                    *span,
                    format!(
                        "identifier '{}' has already been declared in this scope",
                        name
                    ),
                    vec![],
                    extra,
                );
                return true;
            }
            None => {
                return false;
            }
        }
    }

    fn diagnose_reintroduced_procedure_or_function(
        &mut self,
        name: &String,
        span: &span::SpanLoc,
        is_procedure: bool,
    ) -> FunctionProcedureDeclarationStatus {
        if let Some(sym_id) = self.scope.lookup_current_scope(&name) {
            let sym = self.ctx.get_symbol(sym_id);
            let sym = sym.borrow();

            match sym.get_kind() {
                SymbolKind::Function if !is_procedure => {
                    if sym.is_defined() {
                        let extra = self.extra_diag_previous_location(&sym);
                        self.diagnostics.add_with_extra(
                            DiagnosticKind::Error,
                            *span,
                            format!("function '{}' has already been defined", name),
                            vec![],
                            extra,
                        );
                        return FunctionProcedureDeclarationStatus::AlreadyDefined;
                    } else {
                        return FunctionProcedureDeclarationStatus::ForwardDeclared;
                    }
                }
                SymbolKind::Procedure if is_procedure => {
                    if sym.is_defined() {
                        let extra = self.extra_diag_previous_location(&sym);
                        self.diagnostics.add_with_extra(
                            DiagnosticKind::Error,
                            *span,
                            format!("procedure '{}' has already been defined", name),
                            vec![],
                            extra,
                        );
                        return FunctionProcedureDeclarationStatus::AlreadyDefined;
                    } else {
                        return FunctionProcedureDeclarationStatus::ForwardDeclared;
                    }
                }
                _ => {
                    // Already declared
                    let extra = self.extra_diag_previous_location(&sym);
                    self.diagnostics.add_with_extra(
                        DiagnosticKind::Error,
                        *span,
                        format!(
                            "identifier '{}' has already been declared in this scope",
                            name
                        ),
                        vec![],
                        extra,
                    );
                    return FunctionProcedureDeclarationStatus::AlreadyDeclared;
                }
            }
        }
        FunctionProcedureDeclarationStatus::NotDeclared
    }

    fn diagnose_reintroduced_procedure(
        &mut self,
        name: &String,
        span: &span::SpanLoc,
    ) -> FunctionProcedureDeclarationStatus {
        self.diagnose_reintroduced_procedure_or_function(name, span, true)
    }

    fn diagnose_reintroduced_function(
        &mut self,
        name: &String,
        span: &span::SpanLoc,
    ) -> FunctionProcedureDeclarationStatus {
        self.diagnose_reintroduced_procedure_or_function(name, span, false)
    }

    fn is_compatible(&self, lhs_type_id: TypeId, rhs_type_id: TypeId) -> bool {
        if self.ctx.type_system.same_type(lhs_type_id, rhs_type_id) {
            return true;
        }

        // lhs is a subrange of rhs, or rhs is a subrange of lhs, or both lhs and rhs are subranges of the same host-type.
        if (self.ctx.type_system.is_subrange_type(lhs_type_id)
            && self
                .ctx
                .type_system
                .same_type(rhs_type_id, self.ctx.type_system.get_host_type(lhs_type_id)))
            || (self.ctx.type_system.is_subrange_type(rhs_type_id)
                && self
                    .ctx
                    .type_system
                    .same_type(lhs_type_id, self.ctx.type_system.get_host_type(rhs_type_id)))
            || (self.ctx.type_system.is_subrange_type(rhs_type_id)
                && self.ctx.type_system.is_subrange_type(lhs_type_id)
                && self.ctx.type_system.same_type(
                    self.ctx.type_system.get_host_type(lhs_type_id),
                    self.ctx.type_system.get_host_type(rhs_type_id),
                ))
        {
            return true;
        }

        // TODO: lhs and rhs are set-types of compatible base-types, and either both lhs and rhs are designated packed or neither lhs nor rhs is designated packed.

        // TODO: lhs and rhs are string-types with the same number of components

        false
    }

    fn is_assignment_compatible(&self, lhs_type_id: TypeId, rhs_type_id: TypeId) -> bool {
        if self.ctx.type_system.same_type(lhs_type_id, rhs_type_id) {
            return self
                .ctx
                .type_system
                .is_valid_component_type_of_file_type(lhs_type_id);
        }

        if self.ctx.type_system.is_real_type(lhs_type_id)
            && self.ctx.type_system.is_integer_type(rhs_type_id)
        {
            return true;
        }

        // types are compatible ordinal types and the value of rhs is in the closed interval of lhs
        if self.ctx.type_system.is_ordinal_type(lhs_type_id)
            && self.ctx.type_system.is_ordinal_type(rhs_type_id)
            && self.is_compatible(lhs_type_id, rhs_type_id)
        {
            // FIXME: There are cases that we might be able to diagnose here statically.
            return true;
        }

        // TODO: types are compatible set types and all the members of rhs are in the closed interval specified by lhs

        // TODO: lhs and rhs are compatible string types

        false
    }

    fn second_needs_conversion_to_first(&self, lhs_type_id: TypeId, rhs_type_id: TypeId) -> bool {
        self.ctx.type_system.is_real_type(lhs_type_id)
            && self.ctx.type_system.is_integer_type(rhs_type_id)
    }

    fn common_arith_type(&self, lhs_ty: TypeId, rhs_ty: TypeId) -> Option<TypeId> {
        if self.ctx.type_system.is_integer_type(lhs_ty)
            && self.ctx.type_system.is_integer_type(rhs_ty)
        {
            return Some(self.ctx.type_system.get_integer_type());
        }

        if self.ctx.type_system.is_real_type(lhs_ty) && self.ctx.type_system.is_real_type(rhs_ty)
            || self.ctx.type_system.is_integer_type(lhs_ty)
                && self.ctx.type_system.is_real_type(rhs_ty)
            || self.ctx.type_system.is_real_type(lhs_ty)
                && self.ctx.type_system.is_integer_type(rhs_ty)
        {
            return Some(self.ctx.type_system.get_real_type());
        }
        None
    }

    fn real_arith_type(&self, lhs_ty: TypeId, rhs_ty: TypeId) -> Option<TypeId> {
        if self.ctx.type_system.is_integer_type(lhs_ty)
            && self.ctx.type_system.is_integer_type(rhs_ty)
            || self.ctx.type_system.is_real_type(lhs_ty)
                && self.ctx.type_system.is_real_type(rhs_ty)
            || self.ctx.type_system.is_integer_type(lhs_ty)
                && self.ctx.type_system.is_real_type(rhs_ty)
            || self.ctx.type_system.is_real_type(lhs_ty)
                && self.ctx.type_system.is_integer_type(rhs_ty)
        {
            return Some(self.ctx.type_system.get_real_type());
        }
        None
    }

    fn both_integer_type(&self, lhs_ty: TypeId, rhs_ty: TypeId) -> Option<TypeId> {
        if self.ctx.type_system.is_integer_type(lhs_ty)
            && self.ctx.type_system.is_integer_type(rhs_ty)
        {
            return Some(self.ctx.type_system.get_integer_type());
        }
        None
    }

    fn both_bool_type(&self, lhs_ty: TypeId, rhs_ty: TypeId) -> Option<TypeId> {
        if self.ctx.type_system.is_bool_type(lhs_ty) && self.ctx.type_system.is_bool_type(rhs_ty) {
            return Some(self.ctx.type_system.get_bool_type());
        }
        None
    }

    fn relational_compatibility(&self, lhs_type_id: TypeId, rhs_type_id: TypeId) -> Option<TypeId> {
        // TODO: set-types
        if self.is_compatible(lhs_type_id, rhs_type_id) {
            return Some(lhs_type_id);
        } else if self.ctx.type_system.is_integer_type(lhs_type_id)
            && self.ctx.type_system.is_real_type(rhs_type_id)
        {
            return Some(rhs_type_id);
        } else if self.ctx.type_system.is_real_type(lhs_type_id)
            || self.ctx.type_system.is_integer_type(rhs_type_id)
        {
            return Some(lhs_type_id);
        }

        None
    }

    fn valid_for_relational(&self, lhs_type_id: TypeId, rhs_type_id: TypeId) -> Option<TypeId> {
        let valid_type = |ty: TypeId| {
            self.ctx.type_system.is_simple_type(ty) // || ty.is_string_type()
        };

        if !valid_type(lhs_type_id) || !valid_type(rhs_type_id) {
            return None;
        }

        self.relational_compatibility(lhs_type_id, rhs_type_id)
    }

    fn valid_for_relational_equality(
        &self,
        lhs_type_id: TypeId,
        rhs_type_id: TypeId,
    ) -> Option<TypeId> {
        let valid_type = |ty: TypeId| {
            self.ctx.type_system.is_simple_type(ty) // || ty.is_string_type()
        };

        if !valid_type(lhs_type_id) || !valid_type(rhs_type_id) {
            return None;
        }

        self.relational_compatibility(lhs_type_id, rhs_type_id)
    }

    fn valid_for_equality(&self, lhs_type_id: TypeId, rhs_type_id: TypeId) -> Option<TypeId> {
        let valid_type = |ty: TypeId| {
            self.ctx.type_system.is_simple_type(ty) // || ty.is_string_type()
        };

        if !valid_type(lhs_type_id) || !valid_type(rhs_type_id) {
            return None;
        }

        self.relational_compatibility(lhs_type_id, rhs_type_id)
    }

    fn diagnose_invalid_binary_operator(
        &mut self,
        id: span::SpanId,
        lhs_ty: TypeId,
        rhs_ty: TypeId,
        operator_name: &str,
        operator_loc: span::SpanLoc,
        lhs_loc: span::SpanLoc,
        rhs_loc: span::SpanLoc,
    ) {
        self.ctx
            .set_ast_type(id, self.ctx.type_system.get_error_type());
        self.diagnostics.add_with_extra(
            DiagnosticKind::Error,
            operator_loc,
            format!(
                "operator '{}' cannot be applied to operands of type {} and {}",
                operator_name,
                self.ctx.type_system.get_type_name(lhs_ty),
                self.ctx.type_system.get_type_name(rhs_ty)
            ),
            vec![lhs_loc, rhs_loc],
            vec![],
        );
    }

    fn diagnose_invalid_unary_operator(
        &mut self,
        id: span::SpanId,
        op_ty: TypeId,
        operator_name: &str,
        operator_loc: span::SpanLoc,
        op_loc: span::SpanLoc,
    ) {
        self.ctx
            .set_ast_type(id, self.ctx.type_system.get_error_type());
        self.diagnostics.add_with_extra(
            DiagnosticKind::Error,
            operator_loc,
            format!(
                "operator '{}' cannot be applied to operand of type {}",
                operator_name,
                self.ctx.type_system.get_type_name(op_ty)
            ),
            vec![op_loc],
            vec![],
        );
    }

    fn unimplemented(&mut self, loc: span::SpanLoc, msg: &str) {
        self.diagnostics.add(
            DiagnosticKind::Error,
            loc,
            format!("sorry, {} not implemented yet", msg),
        )
    }

    fn create_conversion_expr(n: span::Spanned<ast::Expr>) -> span::Spanned<ast::Expr> {
        span::Spanned::new(
            *n.loc(),
            ast::Expr::Conversion(ast::ExprConversion(span::SpannedBox::from(n))),
        )
    }

    fn declare_formal_parameter(
        &mut self,
        name: &span::Spanned<String>,
        param_ty: TypeId,
        kind: ParameterKind,
    ) {
        // FIXME: Check that we are not hiding the function name
        // or redeclaring a previous parameter.
        let mut new_sym = Symbol::new();
        new_sym.set_name(name.get());
        new_sym.set_kind(SymbolKind::Variable);
        new_sym.set_defining_point(*name.loc());
        new_sym.set_type(param_ty);

        new_sym.set_parameter(kind);

        let new_sym_id = self.ctx.new_symbol(new_sym);
        self.scope.add_entry(&name.get().clone(), new_sym_id);

        self.ctx.set_ast_symbol(name.id(), new_sym_id);
    }

    fn common_check_parameters(
        &mut self,
        is_function: bool,
        function_name: &str,
        args: &mut Vec<span::SpannedBox<ast::Expr>>,
        call_loc: span::SpanLoc,
        callee_symbol_id: SymbolId,
    ) -> bool {
        let arguments: Vec<_> = args
            .iter()
            .map(|arg| {
                (
                    arg.id(),
                    *arg.loc(),
                    self.ctx.get_ast_type(arg.id()).unwrap(),
                )
            })
            .collect();

        // If any argument of the call is wrong, give up.
        if arguments
            .iter()
            .any(|arg| self.ctx.type_system.is_error_type(arg.2))
        {
            return false;
        }

        let params = {
            let callee_symbol = self.ctx.get_symbol(callee_symbol_id);
            let callee_symbol = callee_symbol.borrow();
            callee_symbol.get_formal_parameters().unwrap()
        };

        // If any parameter of the function declaration was already wrong, give up.
        if params.iter().any(|param| {
            let param_sym = self.ctx.get_symbol(*param);
            let param_sym = param_sym.borrow();
            self.ctx
                .type_system
                .is_error_type(param_sym.get_type().unwrap())
        }) {
            return false;
        }

        if arguments.len() != params.len() {
            self.diagnostics.add(
                DiagnosticKind::Error,
                call_loc,
                format!(
                    "{} '{}' expects {} {} but {} arguments were passed",
                    if is_function { "function" } else { "procedure" },
                    function_name,
                    params.len(),
                    if params.len() == 1 {
                        "parameter"
                    } else {
                        "parameters"
                    },
                    arguments.len()
                ),
            );
            return false;
        }

        let mut argument_error = false;
        for (idx, (arg, param_sym_id)) in arguments.iter().zip(params).enumerate() {
            let param_sym = self.ctx.get_symbol(param_sym_id);
            let param_sym = param_sym.borrow();
            let param_kind = param_sym.get_parameter().unwrap();
            let param_type_id = param_sym.get_type().unwrap();
            match param_kind {
                ParameterKind::Value => {
                    if !self.is_assignment_compatible(param_type_id, arg.2) {
                        self.diagnostics.add(
                                                DiagnosticKind::Error,
                                                arg.1,
                                                format!(
                                                    "argument has type {} that is not assignment compatible with value parameter '{}' of type {}",
                                                    self.ctx.type_system.get_type_name(arg.2),
                                                    param_sym.get_name(),
                                                    self.ctx.type_system.get_type_name(param_type_id)
                                                ),
                                            );
                        argument_error = true;
                    }
                }
                ParameterKind::Variable => {
                    let arg_sym = self.ctx.get_ast_symbol(arg.0);

                    let expr_is_variable = match args[idx].get() {
                        ast::Expr::Variable(_) => true,
                        _ => false,
                    };

                    if arg_sym.is_none() || !expr_is_variable {
                        self.diagnostics.add(
                                                DiagnosticKind::Error,
                                                arg.1,
                                                format!(
                                                    "argument is not a variable, as required by variable parameter '{}'",
                                                    param_sym.get_name()
                                                ),
                                            );
                        argument_error = true;
                    } else if !self.ctx.type_system.same_type(param_type_id, arg.2) {
                        self.diagnostics.add(
                                                DiagnosticKind::Error,
                                                arg.1,
                                                format!(
                                                    "argument has type {} but it is different to variable parameter '{}' of type {}",
                                                    self.ctx.type_system.get_type_name(arg.2),
                                                    param_sym.get_name(),
                                                    self.ctx.type_system.get_type_name(param_type_id)
                                                ),
                                            );
                        argument_error = true;
                    } else {
                        assert!(expr_is_variable);
                        let argument = args[idx].get_mut();
                        match argument {
                            ast::Expr::Variable(expr_variable) => {
                                let assig = &mut expr_variable.0;
                                let assig = assig.take();
                                let var_reference = ast::Expr::VariableReference(
                                    ExprVariableReference(span::SpannedBox::from(assig)),
                                );
                                *argument = var_reference;
                            }
                            _ => {
                                panic!("Unexpected tree");
                            }
                        }
                    }
                }
            }
        }

        argument_error
    }

    fn contains_invalid_type_cycle_impl(&self, top_level: bool, root_ty: TypeId, ty: TypeId) -> bool {
        if !top_level && self.ctx.type_system.same_type(root_ty, ty) {
            return true;
        } else if self.ctx.type_system.is_error_type(ty) {
            return false;
        } else if self.ctx.type_system.is_array_type(ty) {
            return self.contains_invalid_type_cycle_impl(
                false,
                root_ty,
                self.ctx.type_system.array_type_get_component_type(ty),
            );
        } else if self.ctx.type_system.is_record_type(ty) {
            let fields = self.ctx.type_system.record_type_get_fields(ty);
            for field in fields {
                let field_sym = self.ctx.get_symbol(*field);
                let field_sym = field_sym.borrow();
                if self.contains_invalid_type_cycle_impl(false, root_ty, field_sym.get_type().unwrap()) {
                    return true;
                }
            }
        }
        // else if self.ctx.type_system.is_pointer_type(ty) {
        //     return false;
        // }
        false
    }

    fn contains_invalid_type_cycle(&self, root_ty: TypeId) -> bool {
        self.contains_invalid_type_cycle_impl(true, root_ty, root_ty)
    }
}

impl<'a> MutatingVisitorMut for SemanticCheckerVisitor<'a> {
    fn visit_program_heading(
        &mut self,
        node: &mut ast::ProgramHeading,
        span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        let x = &node.1;
        x.iter().for_each(|s| {
            // The presence of "input" / "output" makes them a defining point.
            // They are files.
            if s.get() == "input" || s.get() == "output" {
                let mut new_sym = Symbol::new();
                new_sym.set_name(s.get().as_str());
                new_sym.set_kind(SymbolKind::Variable);
                // FIXME: Set their types to files!!!
                new_sym.set_defining_point(*span);

                let new_sym = self.ctx.new_symbol(new_sym);
                self.scope.add_entry(s.get().as_str(), new_sym);
            } else {
                self.lookup_symbol(s.get().as_str(), s.loc());
            }
        });
    }

    fn visit_pre_block(
        &mut self,
        _n: &mut ast::Block,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        self.scope.push_scope(None);
        true
    }

    fn visit_post_block(&mut self, _n: &mut ast::Block, _span: &span::SpanLoc, _id: span::SpanId) {
        self.scope.pop_scope();
    }

    fn visit_label_declaration_part(
        &mut self,
        _n: &mut ast::LabelDeclarationPart,
        span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        self.unimplemented(*span, "declaration of labels");
    }

    fn visit_post_constant_definition(
        &mut self,
        n: &mut ast::ConstantDefinition,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        let name = n.0.get();
        let const_ty = self.ctx.get_ast_type(n.1.id()).unwrap();

        if self.diagnose_redeclared_symbol(name, n.0.loc()) {
            return;
        }

        let mut new_sym = Symbol::new();
        new_sym.set_name(name);
        new_sym.set_kind(SymbolKind::Const);
        new_sym.set_defining_point(*n.0.loc());
        new_sym.set_type(const_ty);

        if let Some(val) = self.ctx.get_ast_value(n.1.id()) {
            new_sym.set_const(val);
        } else {
            assert!(self.ctx.type_system.is_error_type(const_ty));
        }

        let new_sym = self.ctx.new_symbol(new_sym);
        self.scope.add_entry(name, new_sym);
        self.ctx.set_ast_symbol(n.0.id(), new_sym);
    }

    fn visit_pre_type_definition(
        &mut self,
        n: &mut ast::TypeDefinition,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        // We do this in the pre visitor because we want to diagnose
        // redeclarations in the right order.
        let name = n.0.get();
        if self.diagnose_redeclared_symbol(name, n.0.loc()) {
            return false;
        }

        let mut new_sym = Symbol::new();
        new_sym.set_name(name);
        new_sym.set_kind(SymbolKind::Type);
        new_sym.set_defining_point(*n.0.loc());
        // Set a placeholder type until we can set the right type.
        // FIXME: For recursive definitions involving pointers
        // we need to think a bit more.
        new_sym.set_type(self.ctx.type_system.get_error_type());

        let new_sym = self.ctx.new_symbol(new_sym);
        self.scope.add_entry(name, new_sym);
        self.ctx.set_ast_symbol(n.0.id(), new_sym);

        // Walk the type definition itself.
        let type_def_loc = *n.1.loc();
        let type_def_id = n.1.id();
        n.1.get_mut()
            .mutating_walk_mut(self, &type_def_loc, type_def_id);

        let type_denoter = self.ctx.get_ast_type(n.1.id()).unwrap();

        // Update the type stored in the type symbol.
        self.ctx
            .get_symbol_mut(new_sym)
            .borrow_mut()
            .set_type(type_denoter);

        // Check for cycles in the definition.
        if self.contains_invalid_type_cycle(type_denoter) {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *n.1.loc(),
                format!("invalid cyclic reference to type being declared"),
            );
            return false;
        }

        false
    }

    fn visit_enumerated_type(
        &mut self,
        n: &mut ast::EnumeratedType,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let mut enum_ids = vec![];

        let mut enum_error = false;

        for (idx, constant) in (0i64..).zip(n.0.iter()) {
            let constant_name = constant.get();
            if self.diagnose_redeclared_symbol(constant.get(), constant.loc()) {
                enum_error = true;
                continue;
            }

            let mut new_sym = Symbol::new();
            new_sym.set_name(constant_name);
            new_sym.set_kind(SymbolKind::Const);
            new_sym.set_defining_point(*constant.loc());
            new_sym.set_const(Constant::Integer(idx));

            let new_sym = self.ctx.new_symbol(new_sym);
            self.scope.add_entry(constant_name, new_sym);
            self.ctx.set_ast_symbol(constant.id(), new_sym);

            enum_ids.push(new_sym);
        }

        if enum_error {
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
            return;
        }

        let mut enum_type = Type::default();
        enum_type.set_kind(TypeKind::Enum(enum_ids.clone()));

        // Note, technically the enumerators should not be redeclared at all, but for consistency
        // with the way we expect the typesystem work, we will assume it might be possible to create
        // enumerators with the same enumerator ids. Hence the dance below.
        let enum_type_id = self.ctx.type_system.new_type(enum_type);

        // Now link the enumerators to its enum type.
        enum_ids.iter().for_each(|enum_id| {
            self.ctx
                .get_symbol_mut(*enum_id)
                .borrow_mut()
                .set_type(enum_type_id)
        });

        self.ctx.set_ast_type(id, enum_type_id);
    }

    fn visit_post_subrange_type(
        &mut self,
        n: &mut ast::SubrangeType,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let lower = &n.0;
        let upper = &n.1;

        let lower_ty = self.ctx.get_ast_type(lower.id()).unwrap();
        let upper_ty = self.ctx.get_ast_type(lower.id()).unwrap();
        if !self.ctx.type_system.same_type(lower_ty, upper_ty) {
            self.diagnostics.add_with_extra(
                DiagnosticKind::Error,
                *lower.loc(),
                "in a subrange type the two constants must be of the same ordinal type".to_string(),
                vec![*upper.loc()],
                vec![],
            );
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
            return;
        }

        let lower_const = self.ctx.get_ast_value(lower.id()).unwrap();
        let upper_const = self.ctx.get_ast_value(upper.id()).unwrap();

        if !lower_const.le(&upper_const) {
            self.diagnostics.add_with_extra(
                DiagnosticKind::Error,
                *lower.loc(),
                "in a subrange type the first constant must be lower or equal than the second one"
                    .to_string(),
                vec![*upper.loc()],
                vec![],
            );
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
            return;
        }

        // Use the ultimate type as the host for simplicity.
        let ult_type = self.ctx.type_system.ultimate_type(lower_ty);

        let mut subrange_type = Type::default();
        subrange_type.set_kind(TypeKind::SubRange(ult_type, lower_const, upper_const));

        // This deserves a bit of explanation because it is subtle. Two declarations like this
        //   type a = 1..100, b = 1..100;
        // will denote the same TypeId after new_type. This means that the
        // typesystem will assume they are the same type. This is fine for
        // subranges due to their compatibility rules but it will not be for
        // structures as they should have name-equality and not structural
        // equality. Bear this in mind when implementing structures.
        let subrange_type_id = self.ctx.type_system.new_type(subrange_type);
        self.ctx.set_ast_type(id, subrange_type_id);
    }

    fn visit_post_array_type(
        &mut self,
        n: &mut ast::ArrayType,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let packed = n.0.is_some();

        let index_types = &n.1;
        assert!(index_types.len() > 0);

        let component_type = self.ctx.get_ast_type(n.2.id()).unwrap();

        let mut array_type = component_type;

        for ty in index_types.iter().rev() {
            let index = self.ctx.get_ast_type(ty.id()).unwrap();

            let mut new_array = Type::default();
            new_array.set_kind(TypeKind::Array {
                packed,
                index,
                component: array_type,
            });

            array_type = self.ctx.type_system.new_type(new_array);
        }

        self.ctx.set_ast_type(id, array_type);
    }

    fn visit_pre_record_type(
        &mut self,
        _n: &mut ast::RecordType,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        // Create a new scope for the fields
        self.scope.push_scope(None);
        true
    }

    fn visit_post_record_section(
        &mut self,
        n: &mut ast::RecordSection,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        let type_denoter = self.ctx.get_ast_type(n.1.id()).unwrap();

        for field_name in &n.0 {
            if self.diagnose_redeclared_symbol(field_name.get(), field_name.loc()) {
                continue;
            }

            let mut new_sym = Symbol::new();
            new_sym.set_name(field_name.get());
            new_sym.set_kind(SymbolKind::Field);
            new_sym.set_defining_point(*field_name.loc());
            new_sym.set_type(type_denoter);

            let new_sym = self.ctx.new_symbol(new_sym);

            self.scope.add_entry(field_name.get(), new_sym);

            self.ctx.set_ast_symbol(field_name.id(), new_sym);
        }
    }

    fn visit_post_record_type(
        &mut self,
        n: &mut ast::RecordType,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        // Leave the scope of the fields.
        self.scope.pop_scope();

        let packed = n.0.is_some();
        let mut fields: Vec<SymbolId> = vec![];

        // Grab all the symbols.
        for record_section in &n.1 {
            for field_name in &record_section.get().0 {
                if let Some(sym) = self.ctx.get_ast_symbol(field_name.id()) {
                    fields.push(sym);
                }
            }
        }

        let mut new_record_type = Type::default();
        new_record_type.set_kind(TypeKind::Record { packed, fields });

        let new_record_type = self.ctx.type_system.new_type(new_record_type);

        self.ctx.set_ast_type(id, new_record_type);
    }

    fn visit_pre_set_type(
        &mut self,
        _n: &mut ast::SetType,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.unimplemented(*span, "set types");
        self.ctx
            .set_ast_type(id, self.ctx.type_system.get_error_type());
        false
    }

    fn visit_pre_file_type(
        &mut self,
        _n: &mut ast::FileType,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.unimplemented(*span, "file types");
        self.ctx
            .set_ast_type(id, self.ctx.type_system.get_error_type());
        false
    }

    fn visit_pre_procedure_forward(
        &mut self,
        _n: &mut ast::ProcedureForward,
        span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        self.unimplemented(*span, "forward-declaration of procedures");
        false
    }

    fn visit_pre_function_forward(
        &mut self,
        _n: &mut ast::FunctionForward,
        span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        self.unimplemented(*span, "forward-declaration of functions");
        false
    }

    fn visit_pre_function_late_definition(
        &mut self,
        _n: &mut ast::FunctionLateDefinition,
        span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        self.unimplemented(*span, "definition of forward-declared functions");
        false
    }

    fn visit_type_identifier(
        &mut self,
        node: &mut ast::TypeIdentifier,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        if let Some(symbol_id) = self.lookup_symbol(node.0.get(), node.0.loc()) {
            let type_name = self.ctx.get_symbol(symbol_id);
            let type_name = type_name.borrow();
            match type_name.get_kind() {
                SymbolKind::Type => {
                    let named_type = if self
                        .ctx
                        .type_system
                        .is_builtin_type_name(type_name.get_name())
                    {
                        type_name.get_type().unwrap()
                    } else {
                        let mut new_type = Type::default();
                        new_type.set_kind(TypeKind::NamedType(type_name.id()));

                        let ty = self.ctx.type_system.new_type(new_type);
                        ty
                    };
                    self.ctx.set_ast_type(id, named_type);
                }
                SymbolKind::ErrorLookup => {
                    self.ctx
                        .set_ast_type(id, self.ctx.type_system.get_error_type());
                }
                _ => {
                    let extra = self.extra_diag_previous_location(&type_name);
                    self.ctx
                        .set_ast_type(id, self.ctx.type_system.get_error_type());
                    self.diagnostics.add_with_extra(
                        DiagnosticKind::Error,
                        *span,
                        format!(
                            "identifier '{}' has not been declared as a type in this scope",
                            node.0.get()
                        ),
                        vec![],
                        extra,
                    );
                }
            }
        } else {
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
        }
    }

    fn visit_post_variable_declaration(
        &mut self,
        n: &mut ast::VariableDeclaration,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        let ty = self.ctx.get_ast_type(n.1.id()).unwrap();
        n.0.iter().for_each(|e| {
            if self.diagnose_redeclared_symbol(e.get(), e.loc()) {
                return;
            }

            let mut new_sym = Symbol::new();
            new_sym.set_name(e.get());
            new_sym.set_kind(SymbolKind::Variable);
            new_sym.set_defining_point(*e.loc());
            new_sym.set_type(ty.clone());

            let new_sym = self.ctx.new_symbol(new_sym);

            self.scope.add_entry(e.get(), new_sym);

            self.ctx.set_ast_symbol(e.id(), new_sym);
        });
    }

    fn visit_assig_variable(
        &mut self,
        node: &mut ast::AssigVariable,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        if let Some(sym_id) = self.lookup_symbol(node.0.get(), node.0.loc()) {
            let (sym_kind, sym_type) = {
                let var_name = self.ctx.get_symbol(sym_id);
                let var_name = var_name.borrow();
                (var_name.get_kind(), var_name.get_type())
            };

            match sym_kind {
                SymbolKind::Variable => {
                    self.ctx.set_ast_symbol(id, sym_id);
                    self.ctx.set_ast_type(id, sym_type.unwrap());
                }
                SymbolKind::ErrorLookup => {
                    self.ctx
                        .set_ast_type(id, self.ctx.type_system.get_error_type());
                }
                _ => {
                    let extra = {
                        let var_name = self.ctx.get_symbol(sym_id);
                        let var_name = var_name.borrow();
                        self.extra_diag_previous_location(&var_name)
                    };
                    self.diagnostics.add_with_extra(
                        DiagnosticKind::Error,
                        *node.0.loc(),
                        format!(
                            "identifier '{}' has not been declared as a variable in this scope",
                            node.0.get()
                        ),
                        vec![],
                        extra,
                    );
                    self.ctx
                        .set_ast_type(id, self.ctx.type_system.get_error_type());
                }
            }
        } else {
            // The error is already diagnosed during the lookup.
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
        }
    }

    fn visit_post_assig_array_access(
        &mut self,
        n: &mut ast::AssigArrayAccess,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        //
        let lhs_ty = self.ctx.get_ast_type(n.0.id()).unwrap();

        if self.ctx.type_system.is_error_type(lhs_ty) {
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
            return;
        }

        let mut loc_access: span::SpanLoc = *n.0.loc();

        let mut current_ty = lhs_ty;
        let indices = &n.1;
        for index in indices.iter() {
            if !self.ctx.type_system.is_array_type(current_ty) {
                let extra = Diagnostic::new(
                    DiagnosticKind::Info,
                    *index.loc(),
                    format!("first invalid index",),
                );
                self.diagnostics.add_with_extra(
                    DiagnosticKind::Error,
                    loc_access,
                    format!("variable does not have array type"),
                    vec![],
                    vec![extra],
                );
                self.ctx
                    .set_ast_type(id, self.ctx.type_system.get_error_type());
                return;
            }

            loc_access.1 = index.loc().1;

            current_ty = self
                .ctx
                .type_system
                .array_type_get_component_type(current_ty);
        }

        self.ctx.set_ast_type(id, current_ty);
    }

    fn visit_post_assig_field_access(
        &mut self,
        n: &mut ast::AssigFieldAccess,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        //
        let base_type = self.ctx.get_ast_type(n.0.id()).unwrap();
        if self.ctx.type_system.is_error_type(base_type) {
            self.ctx.set_ast_type(id, base_type);
            return;
        }

        if !self.ctx.type_system.is_record_type(base_type) {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *n.0.loc(),
                format!(
                    "expression has type {} which is not a record type",
                    self.ctx.type_system.get_type_name(base_type)
                ),
            );
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
            return;
        }

        let current_field_name = n.1.get();
        let record_fields = self.ctx.type_system.record_type_get_fields(base_type);
        if let Some(named_field) = record_fields.iter().find(|record_field| {
            let record_field_sym = self.ctx.get_symbol(**record_field);
            let record_field_sym = record_field_sym.borrow();

            current_field_name.to_ascii_lowercase()
                == record_field_sym.get_name().to_ascii_lowercase()
        }) {
            //
            let named_field_sym = self.ctx.get_symbol(*named_field);
            let named_field_sym = named_field_sym.borrow();

            self.ctx.set_ast_symbol(n.1.id(), *named_field);
            self.ctx
                .set_ast_type(id, named_field_sym.get_type().unwrap());
        } else {
            //
            self.diagnostics.add(
                DiagnosticKind::Error,
                *n.1.loc(),
                format!(
                    "'{}' is not a field of type {}",
                    current_field_name,
                    self.ctx.type_system.get_type_name(base_type)
                ),
            );
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
            return;
        }
    }

    fn visit_pre_assig_pointer_deref(
        &mut self,
        _n: &mut ast::AssigPointerDeref,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.unimplemented(*span, "pointer dereference");
        self.ctx
            .set_ast_type(id, self.ctx.type_system.get_error_type());
        false
    }

    fn visit_pre_expr(
        &mut self,
        n: &mut ast::Expr,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        // Mutate ExprVariable to be ExprConstant or ExprFunctionCall as needed.
        match n {
            ast::Expr::Variable(assig) => {
                let assig = assig.0.get();
                match assig {
                    ast::Assig::Variable(x) => {
                        if let Some(sym_id) = self.scope.lookup(x.0.get()) {
                            let (sym_kind, sym_type, sym_const, formal_parameters) = {
                                let var_name = self.ctx.get_symbol(sym_id);
                                let var_name = var_name.borrow();
                                (
                                    var_name.get_kind(),
                                    var_name.get_type(),
                                    var_name.get_const(),
                                    var_name.get_formal_parameters(),
                                )
                            };
                            match sym_kind {
                                SymbolKind::Const => {
                                    let new_const_named = span::SpannedBox::new(
                                        *span,
                                        ast::Const::Named(ast::ConstNamed(span::Spanned::new(
                                            *span,
                                            x.0.get().clone(),
                                        ))),
                                    );
                                    self.ctx.set_ast_symbol(new_const_named.id(), sym_id);
                                    self.ctx
                                        .set_ast_type(new_const_named.id(), sym_type.unwrap());
                                    if let Some(sym_const) = &sym_const {
                                        self.ctx
                                            .set_ast_value(new_const_named.id(), sym_const.clone());
                                    } else {
                                        assert!(self
                                            .ctx
                                            .type_system
                                            .is_error_type(sym_type.unwrap()));
                                    }

                                    let new_const =
                                        ast::Expr::Const(ast::ExprConst(new_const_named));
                                    self.ctx.set_ast_symbol(id, sym_id);
                                    self.ctx.set_ast_type(id, sym_type.unwrap());
                                    if let Some(sym_const) = sym_const {
                                        self.ctx.set_ast_value(id, sym_const);
                                    }
                                    *n = new_const;
                                    // Don't traverse further.
                                    return false;
                                }
                                SymbolKind::Function => {
                                    if formal_parameters.unwrap().is_empty() {
                                        // This may be a function call if the function has zero formal parameters.
                                        let new_call_expr = ast::ExprFunctionCall(
                                            span::Spanned::new(*x.0.loc(), x.0.get().clone()),
                                            vec![],
                                        );
                                        self.ctx.set_ast_symbol(new_call_expr.0.id(), sym_id);

                                        let new_call = ast::Expr::FunctionCall(new_call_expr);

                                        let return_symbol_id = {
                                            let callee_symbol = self.ctx.get_symbol(sym_id);
                                            let callee_symbol = callee_symbol.borrow();
                                            callee_symbol.get_return_symbol().unwrap()
                                        };
                                        let return_symbol = self.ctx.get_symbol(return_symbol_id);
                                        let return_symbol = return_symbol.borrow();
                                        self.ctx
                                            .set_ast_type(id, return_symbol.get_type().unwrap());

                                        *n = new_call;
                                        return false;
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        true
    }

    fn visit_pre_expr_range(
        &mut self,
        _n: &mut ast::ExprRange,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.unimplemented(*span, "range expressions");
        self.ctx
            .set_ast_type(id, self.ctx.type_system.get_error_type());
        false
    }

    fn visit_post_expr_variable(
        &mut self,
        node: &mut ast::ExprVariable,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        if let Some(sym) = self.ctx.get_ast_symbol(node.0.id()) {
            self.ctx.set_ast_symbol(id, sym);
        }
        if let Some(ty) = self.ctx.get_ast_type(node.0.id()) {
            self.ctx.set_ast_type(id, ty);
        }
        if let Some(val) = self.ctx.get_ast_value(node.0.id()) {
            self.ctx.set_ast_value(id, val);
        }
    }

    fn visit_pre_stmt_assignment(
        &mut self,
        node: &mut ast::StmtAssignment,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        // Handle lhs or walk it.
        let mut must_walk_lhs = true;
        let lhs = &mut node.0;
        if let Some(scope_symbol_id) = self.scope.get_innermost_scope_symbol() {
            let scope_symbol = self.ctx.get_symbol(scope_symbol_id);
            let scope_symbol = scope_symbol.borrow();
            if scope_symbol.get_kind() == SymbolKind::Function {
                match lhs.get_mut() {
                    ast::Assig::Variable(assig_var) => {
                        let name = assig_var.0.get();
                        if let Some(symbol_id) = self.scope.lookup(&name) {
                            if symbol_id == scope_symbol_id {
                                // This is an assignment to the return variable. Fixup.
                                let return_id = scope_symbol.get_return_symbol().unwrap();
                                let return_sym = self.ctx.get_symbol(return_id);
                                let return_sym = return_sym.borrow();
                                let return_type = return_sym.get_type().unwrap();

                                self.ctx.set_ast_symbol(assig_var.0.id(), return_id);
                                self.ctx.set_ast_type(assig_var.0.id(), return_type);

                                self.ctx.set_ast_symbol(lhs.id(), return_id);
                                self.ctx.set_ast_type(lhs.id(), return_type);
                                must_walk_lhs = false;
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        let lhs = &mut node.0;
        let lhs_loc = *lhs.loc();
        let lhs_id = lhs.id();
        if must_walk_lhs {
            lhs.get_mut().mutating_walk_mut(self, &lhs_loc, lhs_id);
        }

        // Walk rhs
        let rhs = &mut node.1;
        let rhs_loc = *rhs.loc();
        let rhs_id = rhs.id();
        rhs.get_mut().mutating_walk_mut(self, &rhs_loc, rhs_id);

        let lhs_type = self.ctx.get_ast_type(node.0.id()).unwrap();
        let rhs_type = self.ctx.get_ast_type(rhs.id()).unwrap();

        if let Some(assig_sym_id) = self.ctx.get_ast_symbol(node.0.id()) {
            let assig_sym = self.ctx.get_symbol(assig_sym_id);
            let assig_sym = assig_sym.borrow();
            if assig_sym.get_kind() != SymbolKind::Variable {
                self.diagnostics.add(
                    DiagnosticKind::Error,
                    *node.0.loc(),
                    format!("this is not a variable"),
                );
                self.ctx
                    .set_ast_type(id, self.ctx.type_system.get_error_type());
                return false;
            }
        }

        let neither_is_err_type = {
            !self.ctx.type_system.is_error_type(lhs_type)
                && !self.ctx.type_system.is_error_type(rhs_type)
        };

        if neither_is_err_type {
            if self.is_assignment_compatible(lhs_type, rhs_type) {
                self.ctx.set_ast_type(id, lhs_type);
                if self.second_needs_conversion_to_first(lhs_type, rhs_type) {
                    let conversion = SemanticCheckerVisitor::create_conversion_expr(rhs.take());
                    rhs.reset(conversion);
                    self.ctx.set_ast_type(rhs.id(), lhs_type);
                }
            } else {
                self.ctx
                    .set_ast_type(id, self.ctx.type_system.get_error_type());
                let lhs_type_name = self.ctx.type_system.get_type_name(lhs_type);
                let rhs_type_name = self.ctx.type_system.get_type_name(rhs_type);
                self.diagnostics.add_with_extra(DiagnosticKind::Error, lhs_loc,
                format!("left-hand side of this assignment has type {} that is not assignment-compatible with the type {} of the right-hand side", lhs_type_name, rhs_type_name),
            vec![rhs_loc], vec![]);
            }
        } else {
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
        }

        false
    }

    fn visit_post_stmt_if(
        &mut self,
        node: &mut ast::StmtIf,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        let expr_ty = self.ctx.get_ast_type(node.0.id()).unwrap();

        if !self.ctx.type_system.is_bool_type(expr_ty)
            && !self.ctx.type_system.is_error_type(expr_ty)
        {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *node.0.loc(),
                format!("the expression of an if-statement must be of boolean type"),
            );
        }
    }

    fn visit_post_stmt_for(
        &mut self,
        node: &mut ast::StmtFor,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        let var = node.1.id();
        let var_ty_id = self.ctx.get_ast_type(var).unwrap();
        if !self.ctx.type_system.is_error_type(var_ty_id) {
            if !self.ctx.type_system.is_ordinal_type(var_ty_id) {
                let sym_id = self.ctx.get_ast_symbol(node.1.id()).unwrap();
                let extra = {
                    let var_name = self.ctx.get_symbol(sym_id);
                    let var_name = var_name.borrow();
                    self.extra_diag_previous_location(&var_name)
                };
                self.diagnostics.add_with_extra(
                    DiagnosticKind::Error,
                    *node.1.loc(),
                    format!("the control-variable of a for-statement must be of ordinal type"),
                    vec![],
                    extra,
                );
            } else {
                let mut check_value = |value_name: &str, id: span::SpanId, loc: span::SpanLoc| {
                    let ty = self.ctx.get_ast_type(id).unwrap();
                    if !self.is_assignment_compatible(var_ty_id, ty) {
                        self.diagnostics.add(
                            DiagnosticKind::Error,
                            loc,
                            format!(
                                "{} value expression is not assignment-compatible with the control-variable of the for-statement",
                                value_name
                            ),
                        );
                    }
                };

                check_value("initial", node.2.id(), *node.2.loc());
                check_value("final", node.3.id(), *node.3.loc());
            }
        }
    }

    fn visit_post_stmt_while_do(
        &mut self,
        node: &mut ast::StmtWhileDo,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        let expr_ty = self.ctx.get_ast_type(node.0.id()).unwrap();

        if !self.ctx.type_system.is_bool_type(expr_ty) {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *node.0.loc(),
                format!("the expression of a while-statement must be of boolean type"),
            );
        }
    }

    fn visit_post_stmt_repeat_until(
        &mut self,
        node: &mut ast::StmtRepeatUntil,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        let expr_ty = self.ctx.get_ast_type(node.1.id()).unwrap();

        if !self.ctx.type_system.is_bool_type(expr_ty) {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *node.1.loc(),
                format!("the expression of a repeat-statement must be of boolean type"),
            );
        }
    }

    fn visit_post_expr_bin_op(
        &mut self,
        node: &mut ast::ExprBinOp,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let lhs_ty = self.ctx.get_ast_type(node.1.id()).unwrap();
        let rhs_ty = self.ctx.get_ast_type(node.2.id()).unwrap();

        if self.ctx.type_system.is_error_type(lhs_ty) || self.ctx.type_system.is_error_type(rhs_ty)
        {
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
            return;
        }

        match node.0.get() {
            BinOperand::GreaterOrEqualThan | BinOperand::LowerOrEqualThan => {
                let op_type = self.valid_for_relational_equality(lhs_ty, rhs_ty);
                match op_type {
                    Some(operand_type) => {
                        if self.second_needs_conversion_to_first(operand_type, lhs_ty) {
                            assert!(
                                !self.second_needs_conversion_to_first(operand_type, rhs_ty),
                                "there is an extra conversion happening in the RHS?"
                            );
                            let conversion =
                                SemanticCheckerVisitor::create_conversion_expr(node.1.take());
                            node.1.reset(conversion);
                            self.ctx.set_ast_type(node.1.id(), operand_type);
                        } else if self.second_needs_conversion_to_first(operand_type, rhs_ty) {
                            let conversion =
                                SemanticCheckerVisitor::create_conversion_expr(node.2.take());
                            node.2.reset(conversion);
                            self.ctx.set_ast_type(node.2.id(), operand_type);
                        }
                        self.ctx
                            .set_ast_type(id, self.ctx.type_system.get_bool_type());
                    }
                    None => {
                        self.diagnose_invalid_binary_operator(
                            id,
                            lhs_ty,
                            rhs_ty,
                            &node.0.get().to_string(),
                            *node.0.loc(),
                            *node.1.loc(),
                            *node.2.loc(),
                        );
                    }
                }
            }
            BinOperand::GreaterThan | BinOperand::LowerThan => {
                let op_type = self.valid_for_relational(lhs_ty, rhs_ty);
                match op_type {
                    Some(operand_type) => {
                        if self.second_needs_conversion_to_first(operand_type, lhs_ty) {
                            assert!(
                                !self.second_needs_conversion_to_first(operand_type, rhs_ty),
                                "there is an extra conversion happening in the RHS?"
                            );
                            let conversion =
                                SemanticCheckerVisitor::create_conversion_expr(node.1.take());
                            node.1.reset(conversion);
                            self.ctx.set_ast_type(node.1.id(), operand_type);
                        } else if self.second_needs_conversion_to_first(operand_type, rhs_ty) {
                            let conversion =
                                SemanticCheckerVisitor::create_conversion_expr(node.2.take());
                            node.2.reset(conversion);
                            self.ctx.set_ast_type(node.2.id(), operand_type);
                        }
                        self.ctx
                            .set_ast_type(id, self.ctx.type_system.get_bool_type());
                    }
                    None => {
                        self.diagnose_invalid_binary_operator(
                            id,
                            lhs_ty,
                            rhs_ty,
                            &node.0.get().to_string(),
                            *node.0.loc(),
                            *node.1.loc(),
                            *node.2.loc(),
                        );
                    }
                }
            }
            BinOperand::Equal | BinOperand::Different => {
                let op_type = self.valid_for_equality(lhs_ty, rhs_ty);
                match op_type {
                    Some(operand_type) => {
                        if self.second_needs_conversion_to_first(operand_type, lhs_ty) {
                            assert!(
                                !self.second_needs_conversion_to_first(operand_type, rhs_ty),
                                "there is an extra conversion happening in the RHS?"
                            );
                            let conversion =
                                SemanticCheckerVisitor::create_conversion_expr(node.1.take());
                            node.1.reset(conversion);
                            self.ctx.set_ast_type(node.1.id(), operand_type);
                        } else if self.second_needs_conversion_to_first(operand_type, rhs_ty) {
                            let conversion =
                                SemanticCheckerVisitor::create_conversion_expr(node.2.take());
                            node.2.reset(conversion);
                            self.ctx.set_ast_type(node.2.id(), operand_type);
                        }
                        self.ctx
                            .set_ast_type(id, self.ctx.type_system.get_bool_type());
                    }
                    None => {
                        self.diagnose_invalid_binary_operator(
                            id,
                            lhs_ty,
                            rhs_ty,
                            &node.0.get().to_string(),
                            *node.0.loc(),
                            *node.1.loc(),
                            *node.2.loc(),
                        );
                    }
                }
            }
            BinOperand::InSet => {
                self.unimplemented(*span, "set membership operator");
                self.ctx
                    .set_ast_type(id, self.ctx.type_system.get_error_type());
            }
            BinOperand::Addition | BinOperand::Subtraction | BinOperand::Multiplication => {
                let op_type = self.common_arith_type(lhs_ty, rhs_ty);
                match op_type {
                    Some(result_type) => {
                        if self.second_needs_conversion_to_first(result_type, lhs_ty) {
                            assert!(
                                !self.second_needs_conversion_to_first(result_type, rhs_ty),
                                "there is an extra conversion happening in the RHS?"
                            );
                            let conversion =
                                SemanticCheckerVisitor::create_conversion_expr(node.1.take());
                            node.1.reset(conversion);
                            self.ctx.set_ast_type(node.1.id(), result_type);
                        } else if self.second_needs_conversion_to_first(result_type, rhs_ty) {
                            let conversion =
                                SemanticCheckerVisitor::create_conversion_expr(node.2.take());
                            node.2.reset(conversion);
                            self.ctx.set_ast_type(node.2.id(), result_type);
                        }
                        self.ctx.set_ast_type(id, result_type);
                    }
                    None => {
                        // TODO: Set-type operations.
                        self.diagnose_invalid_binary_operator(
                            id,
                            lhs_ty,
                            rhs_ty,
                            &node.0.get().to_string(),
                            *node.0.loc(),
                            *node.1.loc(),
                            *node.2.loc(),
                        );
                    }
                }
            }
            BinOperand::LogicalOr | BinOperand::LogicalAnd => {
                let op_type = self.both_bool_type(lhs_ty, rhs_ty);
                match op_type {
                    Some(result_type) => {
                        self.ctx.set_ast_type(id, result_type);
                    }
                    None => {
                        // TODO: Set-type operations.
                        self.diagnose_invalid_binary_operator(
                            id,
                            lhs_ty,
                            rhs_ty,
                            &node.0.get().to_string(),
                            *node.0.loc(),
                            *node.1.loc(),
                            *node.2.loc(),
                        );
                    }
                }
            }
            BinOperand::RealDivision => {
                let op_type = self.real_arith_type(lhs_ty, rhs_ty);
                match op_type {
                    Some(result_type) => {
                        if self.second_needs_conversion_to_first(result_type, lhs_ty) {
                            let conversion =
                                SemanticCheckerVisitor::create_conversion_expr(node.1.take());
                            node.1.reset(conversion);
                            self.ctx.set_ast_type(node.1.id(), result_type);
                        }
                        if self.second_needs_conversion_to_first(result_type, rhs_ty) {
                            let conversion =
                                SemanticCheckerVisitor::create_conversion_expr(node.2.take());
                            node.2.reset(conversion);
                            self.ctx.set_ast_type(node.2.id(), result_type);
                        }
                        self.ctx.set_ast_type(id, result_type);
                    }
                    None => {
                        // TODO: Set-type operations.
                        self.diagnose_invalid_binary_operator(
                            id,
                            lhs_ty,
                            rhs_ty,
                            &node.0.get().to_string(),
                            *node.0.loc(),
                            *node.1.loc(),
                            *node.2.loc(),
                        );
                    }
                }
            }
            BinOperand::IntegerDivision | BinOperand::Modulo => {
                let op_type = self.both_integer_type(lhs_ty, rhs_ty);
                match op_type {
                    Some(result_type) => {
                        self.ctx.set_ast_type(id, result_type);
                    }
                    None => {
                        // TODO: Set-type operations.
                        self.diagnose_invalid_binary_operator(
                            id,
                            lhs_ty,
                            rhs_ty,
                            &node.0.get().to_string(),
                            *node.0.loc(),
                            *node.1.loc(),
                            *node.2.loc(),
                        );
                    }
                }
            }
        };
    }

    fn visit_post_expr_un_op(
        &mut self,
        node: &mut ast::ExprUnOp,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let op_ty_id = self.ctx.get_ast_type(node.1.id()).unwrap();

        if self.ctx.type_system.is_error_type(op_ty_id) {
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
            return;
        }

        match node.0.get() {
            UnaryOp::Negation | UnaryOp::Plus => {
                if self.ctx.type_system.is_integer_type(op_ty_id)
                    || self.ctx.type_system.is_real_type(op_ty_id)
                {
                    self.ctx.set_ast_type(id, op_ty_id);
                } else {
                    self.diagnose_invalid_unary_operator(
                        id,
                        op_ty_id,
                        &node.0.get().to_string(),
                        *node.0.loc(),
                        *node.1.loc(),
                    );
                }
            }
            UnaryOp::LogicalNot => {
                if self.ctx.type_system.is_bool_type(op_ty_id) {
                    self.ctx.set_ast_type(id, op_ty_id);
                } else {
                    self.diagnose_invalid_unary_operator(
                        id,
                        op_ty_id,
                        &node.0.get().to_string(),
                        *node.0.loc(),
                        *node.1.loc(),
                    );
                }
            }
        }
    }

    fn visit_pre_expr_function_call(
        &mut self,
        node: &mut ast::ExprFunctionCall,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        let callee = &node.0;
        let args = &mut node.1;

        if is_required_function(callee.get()) {
            match callee.get() {
                _ => {
                    self.unimplemented(
                        *span,
                        &format!("call to required function '{}'", callee.get()),
                    );
                    self.ctx
                        .set_ast_type(id, self.ctx.type_system.get_error_type());
                    return false;
                }
            }
        } else if is_required_procedure(callee.get()) {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *callee.loc(),
                format!(
                    "required procedure '{}' cannot be referenced in a function call",
                    callee.get()
                ),
            );
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
            return false;
        }

        if let Some(callee_symbol_id) = self.lookup_symbol(callee.get(), callee.loc()) {
            let callee_symbol_kind = self.ctx.get_symbol(callee_symbol_id).borrow().get_kind();
            match callee_symbol_kind {
                SymbolKind::Function => {
                    args.iter_mut().for_each(|arg| {
                        let loc = *arg.loc();
                        let id = arg.id();
                        arg.get_mut().mutating_walk_mut(self, &loc, id);
                    });

                    let argument_error = self.common_check_parameters(
                        true,
                        &callee.get(),
                        args,
                        *span,
                        callee_symbol_id,
                    );

                    if argument_error {
                        self.ctx
                            .set_ast_type(id, self.ctx.type_system.get_error_type());
                        return false;
                    }

                    // All looks good, the expression has type the return type of the function.
                    self.ctx.set_ast_symbol(callee.id(), callee_symbol_id);

                    let return_symbol_id = {
                        let callee_symbol = self.ctx.get_symbol(callee_symbol_id);
                        let callee_symbol = callee_symbol.borrow();
                        callee_symbol.get_return_symbol().unwrap()
                    };

                    let return_symbol = self.ctx.get_symbol(return_symbol_id);
                    let return_symbol = return_symbol.borrow();
                    self.ctx.set_ast_type(id, return_symbol.get_type().unwrap());
                }
                SymbolKind::ErrorLookup => {
                    self.ctx
                        .set_ast_type(id, self.ctx.type_system.get_error_type());
                }
                _ => {
                    let extra = {
                        let var_name = self.ctx.get_symbol(callee_symbol_id);
                        let var_name = var_name.borrow();
                        self.extra_diag_previous_location(&var_name)
                    };
                    self.diagnostics.add_with_extra(
                        DiagnosticKind::Error,
                        *callee.loc(),
                        format!(
                            "identifier '{}' has not been declared as a function in this scope and cannot be called",
                            callee.get()
                        ),
                        vec![],
                        extra,
                    );
                    self.ctx
                        .set_ast_type(id, self.ctx.type_system.get_error_type());
                }
            }
        } else {
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
        }

        false
    }

    fn visit_pre_expr_set_literal(
        &mut self,
        _n: &mut ast::ExprSetLiteral,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.unimplemented(*span, "set literal expressions");
        self.ctx
            .set_ast_type(id, self.ctx.type_system.get_error_type());
        false
    }

    fn visit_post_expr_parentheses(
        &mut self,
        node: &mut ast::ExprParentheses,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        self.ctx
            .set_ast_type(id, self.ctx.get_ast_type(node.0.id()).unwrap());
        if let Some(sym_id) = self.ctx.get_ast_symbol(node.0.id()) {
            self.ctx.set_ast_symbol(id, sym_id);
        }
    }

    fn visit_const_integer(
        &mut self,
        n: &mut ast::ConstInteger,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        self.ctx
            .set_ast_type(id, self.ctx.type_system.get_integer_type());
        self.ctx.set_ast_value(id, Constant::Integer(*n.0.get()));
    }

    fn visit_const_real(
        &mut self,
        n: &mut ast::ConstReal,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        self.ctx
            .set_ast_type(id, self.ctx.type_system.get_real_type());
        self.ctx.set_ast_value(id, Constant::Real(*n.0.get()));
    }

    fn visit_const_named(
        &mut self,
        node: &mut ast::ConstNamed,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        if let Some(sym_id) = self.lookup_symbol(node.0.get(), node.0.loc()) {
            let (sym_kind, sym_type, sym_value) = {
                let var_name = self.ctx.get_symbol(sym_id);
                let var_name = var_name.borrow();
                (
                    var_name.get_kind(),
                    var_name.get_type(),
                    var_name.get_const(),
                )
            };
            match sym_kind {
                SymbolKind::Const => {
                    self.ctx.set_ast_symbol(id, sym_id);
                    self.ctx.set_ast_type(id, sym_type.unwrap());
                    self.ctx.set_ast_value(id, sym_value.unwrap());
                }
                SymbolKind::ErrorLookup => {
                    self.ctx
                        .set_ast_type(id, self.ctx.type_system.get_error_type());
                }
                _ => {}
            }
        }
    }

    fn visit_const_minus_named(
        &mut self,
        node: &mut ast::ConstMinusNamed,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        if let Some(sym_id) = self.lookup_symbol(node.0.get(), node.0.loc()) {
            let (sym_kind, sym_type, sym_value) = {
                let var_name = self.ctx.get_symbol(sym_id);
                let var_name = var_name.borrow();
                (
                    var_name.get_kind(),
                    var_name.get_type(),
                    var_name.get_const(),
                )
            };
            match sym_kind {
                SymbolKind::Const => {
                    self.ctx.set_ast_symbol(id, sym_id);
                    self.ctx.set_ast_type(id, sym_type.unwrap());
                    if !self.ctx.type_system.is_integer_type(sym_type.unwrap())
                        && !self.ctx.type_system.is_real_type(sym_type.unwrap())
                    {
                        self.diagnostics.add(
                            DiagnosticKind::Error,
                            *node.0.loc(),
                            "negation of a constant that is not an integer or a real".to_string(),
                        );
                        self.ctx
                            .set_ast_type(id, self.ctx.type_system.get_error_type());
                        return;
                    }
                    let c = match sym_value.unwrap() {
                        Constant::Integer(x) => Constant::Integer(-x),
                        Constant::Real(x) => Constant::Real(-x),
                        _ => {
                            panic!("Invalid costant");
                        }
                    };
                    self.ctx.set_ast_value(id, c);
                }
                SymbolKind::ErrorLookup => {
                    self.ctx
                        .set_ast_type(id, self.ctx.type_system.get_error_type());
                }
                _ => {}
            }
        } else {
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
        }
    }

    fn visit_const_string_literal(
        &mut self,
        n: &mut ast::ConstStringLiteral,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let str = n.0.get();
        let len = str.chars().count();
        let string_literal_type = if len > 1 {
            self.ctx.type_system.get_string_type(str.chars().count())
        } else {
            self.ctx.type_system.get_char_type()
        };
        self.ctx.set_ast_type(id, string_literal_type);
    }

    fn visit_post_expr_const(
        &mut self,
        node: &mut ast::ExprConst,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        self.ctx
            .set_ast_type(id, self.ctx.get_ast_type(node.0.id()).unwrap());
    }

    fn visit_stmt_goto(&mut self, _n: &mut ast::StmtGoto, span: &span::SpanLoc, _id: span::SpanId) {
        self.unimplemented(*span, "goto-statements");
    }

    fn visit_pre_stmt_label(
        &mut self,
        _n: &mut ast::StmtLabel,
        span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        self.unimplemented(*span, "labeled-statements");
        false
    }

    fn visit_pre_stmt_procedure_call(
        &mut self,
        node: &mut ast::StmtProcedureCall,
        span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        let callee = &node.0;
        let proc_name = callee.get().as_str();
        if is_required_procedure(proc_name) {
            match proc_name {
                "readln" | "writeln" => return true,
                _ => {
                    self.unimplemented(*span, &format!("required procedure '{}'", proc_name));
                    return false;
                }
            }
        }
        true
    }

    fn visit_post_stmt_case(
        &mut self,
        n: &mut ast::StmtCase,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        let case_expr = &n.0;
        let cases = &n.1;

        let mut case_expr_err = false;

        let expr_ty = self.ctx.get_ast_type(case_expr.id()).unwrap();
        if !self.ctx.type_system.is_ordinal_type(expr_ty) {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *case_expr.loc(),
                format!("expression must be of ordinal type",),
            );
            case_expr_err = true;
        }

        let mut const_set = HashMap::new();

        for case in cases {
            let case_ast = case.get();
            let case_consts = &case_ast.0;
            for case_const in case_consts {
                let const_ty = self.ctx.get_ast_type(case_const.id()).unwrap();
                if !self.ctx.type_system.same_type(const_ty, expr_ty) && !case_expr_err {
                    self.diagnostics.add_with_extra(
                        DiagnosticKind::Error,
                        *case_const.loc(),
                        format!("constant of case must be of same ordinal type as case expression"),
                        vec![*case_expr.loc()],
                        vec![],
                    );
                }
                match self.ctx.get_ast_value(case_const.id()).unwrap() {
                    Constant::Integer(x) => {
                        if let Some(prev_loc) = const_set.insert(x, case_const.loc()) {
                            let previous_const = Diagnostic::new(
                                DiagnosticKind::Info,
                                *prev_loc,
                                format!("previous case"),
                            );
                            self.diagnostics.add_with_extra(
                                DiagnosticKind::Error,
                                *case_const.loc(),
                                format!("case repeated"),
                                vec![],
                                vec![previous_const],
                            );
                        }
                    }
                    _ => {
                        panic!("Unexpected constant");
                    }
                }
            }
        }
    }

    fn visit_pre_stmt_with(
        &mut self,
        _n: &mut ast::StmtWith,
        span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        self.unimplemented(*span, "with-statements");
        false
    }

    fn visit_post_expr_write_parameter(
        &mut self,
        node: &mut ast::ExprWriteParameter,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        let mut check_parameter = |param_name: &str, id: span::SpanId, loc: span::SpanLoc| {
            let type_id = self.ctx.get_ast_type(id).unwrap();
            if self.ctx.type_system.is_error_type(type_id) {
                return;
            }

            if !self.ctx.type_system.is_integer_type(type_id) {
                self.diagnostics.add(
                    DiagnosticKind::Error,
                    loc,
                    format!(
                        "parameter {} of a write-parameter must be of integer type",
                        param_name
                    ),
                );
            }
        };

        let mut must_be_real = false;

        check_parameter("total-width", node.1.id(), *node.1.loc());
        if let Some(frac_digits) = &node.2 {
            must_be_real = true;
            check_parameter("frac-digits", frac_digits.id(), *frac_digits.loc());
        }

        let ty = self.ctx.get_ast_type(node.0.id()).unwrap();
        if must_be_real && !self.ctx.type_system.is_real_type(ty) {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *node.0.loc(),
                format!("argument must be of real type because frac-width was specified",),
            );
        } else if !self.ctx.type_system.is_real_type(ty)
            && !self.ctx.type_system.is_bool_type(ty)
            && !self.ctx.type_system.is_integer_type(ty)
        {
            self.diagnostics.add(
                    DiagnosticKind::Error,
                    *node.0.loc(),
                    format!("argument of writeln must be an expression of integer-type, real-type or Boolean-type"),
                );
        }
    }

    fn visit_post_stmt_procedure_call(
        &mut self,
        node: &mut ast::StmtProcedureCall,
        span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        let callee = &node.0;
        let args = &mut node.1;
        let procedure_name = callee.get().as_str();
        if is_required_procedure(procedure_name) {
            match procedure_name {
                "readln" => {
                    if let Some(args) = &node.1 {
                        for arg in args {
                            match arg.get() {
                                ast::Expr::Variable(_) => {}
                                _ => {
                                    self.diagnostics.add(
                                        DiagnosticKind::Error,
                                        *arg.loc(),
                                        format!("must be a variable"),
                                    );
                                }
                            }
                        }
                    }
                }
                "writeln" => {}
                _ => {
                    unreachable!("call to procedure {}", node.0.get().as_str())
                }
            }
        } else if is_required_function(procedure_name) {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *callee.loc(),
                format!(
                    "cannot call required function '{}' like a procedure",
                    procedure_name
                ),
            );
        } else {
            if let Some(callee_symbol_id) = self.lookup_symbol(callee.get(), callee.loc()) {
                let callee_symbol_kind = self.ctx.get_symbol(callee_symbol_id).borrow().get_kind();
                match callee_symbol_kind {
                    SymbolKind::Procedure => {
                        if let Some(args) = args {
                            args.iter_mut().for_each(|arg| {
                                let loc = *arg.loc();
                                let id = arg.id();
                                arg.get_mut().mutating_walk_mut(self, &loc, id);
                            });

                            self.common_check_parameters(
                                false,
                                &callee.get(),
                                args,
                                *span,
                                callee_symbol_id,
                            );
                        }

                        self.ctx.set_ast_symbol(callee.id(), callee_symbol_id);
                    }
                    SymbolKind::ErrorLookup => {
                        //
                    }
                    _ => {
                        let extra = {
                            let var_name = self.ctx.get_symbol(callee_symbol_id);
                            let var_name = var_name.borrow();
                            self.extra_diag_previous_location(&var_name)
                        };
                        self.diagnostics.add_with_extra(
                        DiagnosticKind::Error,
                        *callee.loc(),
                        format!(
                            "identifier '{}' has not been declared as a procedure in this scope and cannot be called",
                            callee.get()
                        ),
                        vec![],
                        extra,
                    );
                    }
                }
            }
        }
    }

    fn visit_pre_function_definition(
        &mut self,
        n: &mut ast::FunctionDefinition,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        let function_name = n.0.get();

        match self.diagnose_reintroduced_function(function_name, n.0.loc()) {
            FunctionProcedureDeclarationStatus::AlreadyDeclared
            | FunctionProcedureDeclarationStatus::AlreadyDefined => {
                // This is an error.
                return false;
            }
            FunctionProcedureDeclarationStatus::ForwardDeclared => {
                // FIXME: Check for compatibility
                self.unimplemented(*n.0.loc(), "forward declared functions");
                return false;
            }
            FunctionProcedureDeclarationStatus::NotDeclared => {
                // Fine
            }
        }

        if is_required_procedure_or_function(&function_name) {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *n.0.loc(),
                format!(
                    "cannot define required {} '{}'",
                    if is_required_function(&function_name) {
                        "function"
                    } else {
                        "procedure"
                    },
                    n.0.get()
                ),
            );
            return false;
        }

        let mut function_sym = Symbol::new();
        function_sym.set_name(function_name);
        function_sym.set_kind(SymbolKind::Function);
        function_sym.set_defining_point(*n.0.loc());

        let function_sym_id = self.ctx.new_symbol(function_sym);
        self.ctx.set_ast_symbol(n.0.id(), function_sym_id);
        self.scope
            .add_entry(&function_name.clone(), function_sym_id);

        self.scope.push_scope(Some(function_sym_id));

        // Formal parameters
        let mut formal_parameters = vec![];
        if let Some(parameters) = &mut n.1 {
            parameters.iter_mut().for_each(|param| {
                let loc = *param.loc();
                let id = param.id();
                param.get_mut().mutating_walk_mut(self, &loc, id);

                let mut register_parameter = |name: &span::Spanned<String>| {
                    formal_parameters.push(self.ctx.get_ast_symbol(name.id()).unwrap());
                };

                match param.get() {
                    FormalParameter::Value(n) => {
                        n.0.iter().for_each(|name| register_parameter(name));
                    }
                    FormalParameter::Variable(n) => {
                        n.0.iter().for_each(|name| register_parameter(name));
                    }
                    _ => {
                        panic!("Not implemented yet");
                    }
                };
            });
        }

        self.ctx
            .get_symbol_mut(function_sym_id)
            .borrow_mut()
            .set_formal_parameters(formal_parameters);

        // TypeId of return
        let result_type = {
            let loc = *n.2.loc();
            let id = n.2.id();
            n.2.get_mut().mutating_walk_mut(self, &loc, id);
            self.ctx.get_ast_type(n.2.id()).unwrap()
        };

        let mut return_symbol = Symbol::new();
        return_symbol.set_name(function_name);
        return_symbol.set_kind(SymbolKind::Variable);
        return_symbol.set_defining_point(*n.0.loc());
        return_symbol.set_type(result_type);

        let return_symbol_id = self.ctx.new_symbol(return_symbol);

        self.ctx
            .get_symbol_mut(function_sym_id)
            .borrow_mut()
            .set_return_symbol(return_symbol_id);

        let function_block = &mut n.3;
        let loc = *function_block.loc();
        let id = function_block.id();
        function_block.get_mut().mutating_walk_mut(self, &loc, id);

        self.scope.pop_scope();

        false
    }

    fn visit_pre_procedure_definition(
        &mut self,
        n: &mut ast::ProcedureDefinition,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        let proc_name = n.0.get();

        match self.diagnose_reintroduced_procedure(proc_name, n.0.loc()) {
            FunctionProcedureDeclarationStatus::AlreadyDeclared
            | FunctionProcedureDeclarationStatus::AlreadyDefined => {
                return false;
            }
            FunctionProcedureDeclarationStatus::ForwardDeclared => {
                // FIXME: Check for compatibility
                self.unimplemented(*n.0.loc(), "forward declared functions");
                return false;
            }
            FunctionProcedureDeclarationStatus::NotDeclared => {
                // Fine
            }
        }

        if is_required_procedure_or_function(&proc_name) {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *n.0.loc(),
                format!(
                    "cannot define required {} '{}'",
                    if is_required_function(&proc_name) {
                        "function"
                    } else {
                        "procedure"
                    },
                    n.0.get()
                ),
            );
            return false;
        }

        let mut proc_sym = Symbol::new();
        proc_sym.set_name(proc_name);
        proc_sym.set_kind(SymbolKind::Procedure);
        proc_sym.set_defining_point(*n.0.loc());

        let proc_sym_id = self.ctx.new_symbol(proc_sym);
        self.ctx.set_ast_symbol(n.0.id(), proc_sym_id);
        self.scope.add_entry(&proc_name.clone(), proc_sym_id);

        self.scope.push_scope(Some(proc_sym_id));

        // Formal parameters
        let mut formal_parameters = vec![];
        if let Some(parameters) = &mut n.1 {
            parameters.iter_mut().for_each(|param| {
                let loc = *param.loc();
                let id = param.id();
                param.get_mut().mutating_walk_mut(self, &loc, id);

                let mut register_parameter = |name: &span::Spanned<String>| {
                    formal_parameters.push(self.ctx.get_ast_symbol(name.id()).unwrap());
                };

                match param.get() {
                    FormalParameter::Value(n) => {
                        n.0.iter().for_each(|name| register_parameter(name));
                    }
                    FormalParameter::Variable(n) => {
                        n.0.iter().for_each(|name| register_parameter(name));
                    }
                    _ => {
                        panic!("Not implemented yet");
                    }
                };
            });
        }

        self.ctx
            .get_symbol_mut(proc_sym_id)
            .borrow_mut()
            .set_formal_parameters(formal_parameters);

        let proc_block = &mut n.2;
        let loc = *proc_block.loc();
        let id = proc_block.id();
        proc_block.get_mut().mutating_walk_mut(self, &loc, id);

        self.scope.pop_scope();

        false
    }

    fn visit_post_formal_parameter_value(
        &mut self,
        n: &mut ast::FormalParameterValue,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        //
        let names = &n.0;
        let type_name = &n.1;

        let param_ty = self.ctx.get_ast_type(type_name.id()).unwrap();

        names.iter().for_each(|name| {
            self.declare_formal_parameter(name, param_ty, ParameterKind::Value);
        });
    }

    fn visit_post_formal_parameter_variable(
        &mut self,
        n: &mut ast::FormalParameterVariable,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        let names = &n.0;
        let type_name = &n.1;

        let param_ty = self.ctx.get_ast_type(type_name.id()).unwrap();

        names.iter().for_each(|name| {
            self.declare_formal_parameter(name, param_ty, ParameterKind::Variable);
        });
    }

    fn visit_pre_formal_parameter_function(
        &mut self,
        _n: &mut ast::FormalParameterFunction,
        span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        self.unimplemented(*span, "function formal parameters");
        false
    }

    fn visit_pre_formal_parameter_procedure(
        &mut self,
        _n: &mut ast::FormalParameterProcedure,
        span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        self.unimplemented(*span, "procedure formal parameters");
        false
    }
}

fn init_global_scope(scope: &mut scope::Scope, semantic_context: &mut SemanticContext) {
    let mut new_sym = Symbol::new();
    new_sym.set_name("integer");
    new_sym.set_kind(SymbolKind::Type);
    new_sym.set_type(semantic_context.type_system.get_integer_type());
    let new_sym = semantic_context.new_symbol(new_sym);
    scope.add_entry("integer", new_sym);

    let mut new_sym = Symbol::new();
    new_sym.set_name("real");
    new_sym.set_kind(SymbolKind::Type);
    new_sym.set_type(semantic_context.type_system.get_real_type());
    let new_sym = semantic_context.new_symbol(new_sym);
    scope.add_entry("real", new_sym);

    let mut new_sym = Symbol::new();
    new_sym.set_name("boolean");
    new_sym.set_kind(SymbolKind::Type);
    new_sym.set_type(semantic_context.type_system.get_bool_type());
    let new_sym = semantic_context.new_symbol(new_sym);
    scope.add_entry("boolean", new_sym);

    let mut new_sym = Symbol::new();
    new_sym.set_name("char");
    new_sym.set_kind(SymbolKind::Type);
    new_sym.set_type(semantic_context.type_system.get_char_type());
    let new_sym = semantic_context.new_symbol(new_sym);
    scope.add_entry("char", new_sym);

    let mut new_sym = Symbol::new();
    new_sym.set_name("true");
    new_sym.set_kind(SymbolKind::Const);
    new_sym.set_type(semantic_context.type_system.get_bool_type());
    new_sym.set_const(Constant::Bool(true));
    let new_sym = semantic_context.new_symbol(new_sym);
    scope.add_entry("true", new_sym);

    let mut new_sym = Symbol::new();
    new_sym.set_name("false");
    new_sym.set_kind(SymbolKind::Const);
    new_sym.set_type(semantic_context.type_system.get_bool_type());
    new_sym.set_const(Constant::Bool(false));
    let new_sym = semantic_context.new_symbol(new_sym);
    scope.add_entry("false", new_sym);
}

pub fn check_program(
    program: &mut span::SpannedBox<ast::Program>,
    semantic_context: &mut SemanticContext,
    diagnostics: &mut Diagnostics,
    scope: &mut scope::Scope,
) {
    // Init global scope.
    init_global_scope(scope, semantic_context);

    let mut checker_visitor = SemanticCheckerVisitor {
        ctx: semantic_context,
        diagnostics,
        scope,
    };

    let loc = *program.loc();
    let id = program.id();
    program
        .get_mut()
        .mutating_walk_mut(&mut checker_visitor, &loc, id);
}

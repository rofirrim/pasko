use crate::ast::{self, ExprVariableReference, UnaryOp};
use crate::ast::{BinOperand, FormalParameter};
use crate::constant::Constant;
use crate::diagnostics::{Diagnostic, DiagnosticKind, Diagnostics};
use crate::span::{self, SpannedBox};
use crate::symbol::{
    ParameterKind, Symbol, SymbolId, SymbolKind, SymbolMap, SymbolMapImpl, SymbolRef,
};
use crate::typesystem::{Type, TypeId, TypeKind, TypeSystem};
use crate::visitor::MutatingVisitable;
use crate::visitor::MutatingVisitorMut;
use crate::{scope, typesystem};

use std::collections::{HashMap, HashSet};

pub struct SemanticContext {
    symbol_map: SymbolMap,
    pub type_system: TypeSystem,

    ast_types: HashMap<span::SpanId, TypeId>,
    ast_symbols: HashMap<span::SpanId, SymbolId>,
    ast_values: HashMap<span::SpanId, Constant>,

    pub scope: scope::Scope,

    pub program_parameters: Vec<(String, span::SpanLoc)>,
    pub global_files: Vec<SymbolId>,
    pending_type_definitions: Vec<SymbolId>,
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

// Returns if the function is a required function that can be called with zero arguments.
pub fn is_required_function_zeroadic(name: &str) -> bool {
    is_required_function(name)
        && match name {
            "eof" | "eoln" => true,
            _ => false,
        }
}

// In bytes.
impl SemanticContext {
    pub fn new() -> SemanticContext {
        let symbol_map = SymbolMapImpl::new();
        let type_system = TypeSystem::new(symbol_map.clone());

        let sc = SemanticContext {
            symbol_map,
            type_system,
            scope: scope::Scope::new(),

            ast_types: HashMap::new(),
            ast_symbols: HashMap::new(),
            ast_values: HashMap::new(),

            program_parameters: vec![],
            global_files: vec![],
            pending_type_definitions: vec![],
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

    pub fn add_to_pending_definitions(&mut self, sym_id: SymbolId) {
        self.pending_type_definitions.push(sym_id);
    }

    pub fn remove_from_pending_definitions(&mut self, sym_id: SymbolId) {
        let index = self
            .pending_type_definitions
            .iter()
            .position(|x| *x == sym_id)
            .unwrap();
        self.pending_type_definitions.remove(index);
    }

    pub fn required_function_zeroadic_return_type(&self, name: &str) -> TypeId {
        debug_assert!(is_required_function_zeroadic(name));
        match name {
            "eof" | "eoln" => self.type_system.get_bool_type(),
            _ => panic!("Unexpected function {}", name),
        }
    }

    fn init_global_scope(&mut self) {
        let mut new_sym = Symbol::new();
        new_sym.set_name("integer");
        new_sym.set_kind(SymbolKind::Type);
        new_sym.set_type(self.type_system.get_integer_type());
        let new_sym = self.new_symbol(new_sym);
        self.scope.add_entry("integer", new_sym);

        let mut new_sym = Symbol::new();
        new_sym.set_name("real");
        new_sym.set_kind(SymbolKind::Type);
        new_sym.set_type(self.type_system.get_real_type());
        let new_sym = self.new_symbol(new_sym);
        self.scope.add_entry("real", new_sym);

        let mut new_sym = Symbol::new();
        new_sym.set_name("boolean");
        new_sym.set_kind(SymbolKind::Type);
        new_sym.set_type(self.type_system.get_bool_type());
        let new_sym = self.new_symbol(new_sym);
        self.scope.add_entry("boolean", new_sym);

        let mut new_sym = Symbol::new();
        new_sym.set_name("char");
        new_sym.set_kind(SymbolKind::Type);
        new_sym.set_type(self.type_system.get_char_type());
        let new_sym = self.new_symbol(new_sym);
        self.scope.add_entry("char", new_sym);

        let mut new_sym = Symbol::new();
        new_sym.set_name("true");
        new_sym.set_kind(SymbolKind::Const);
        new_sym.set_type(self.type_system.get_bool_type());
        new_sym.set_const(Constant::Bool(true));
        let new_sym = self.new_symbol(new_sym);
        self.scope.add_entry("true", new_sym);

        let mut new_sym = Symbol::new();
        new_sym.set_name("false");
        new_sym.set_kind(SymbolKind::Const);
        new_sym.set_type(self.type_system.get_bool_type());
        new_sym.set_const(Constant::Bool(false));
        let new_sym = self.new_symbol(new_sym);
        self.scope.add_entry("false", new_sym);

        let mut new_sym = Symbol::new();
        new_sym.set_name("text");
        new_sym.set_kind(SymbolKind::Type);
        new_sym.set_type(self.type_system.get_textfile_type());
        let new_sym = self.new_symbol(new_sym);
        self.scope.add_entry("text", new_sym);
    }
}

struct SemanticCheckerVisitor<'a> {
    ctx: &'a mut SemanticContext,
    diagnostics: &'a mut Diagnostics,

    in_type_definition_part: bool,
    in_pointer_type: bool,

    program_heading_loc: Option<span::SpanLoc>,

    // Information needed for records.
    record_info: typesystem::VariantPart,
}

enum FunctionProcedureDeclarationStatus {
    /// The identifier has not been declared.
    NotDeclared,
    /// The identifier has been forward declared as a compatible symbol kind and
    /// should be checked for compatibility against its definition.
    ForwardDeclared(SymbolId),
    /// Already declared as something else. This is an error.
    AlreadyDeclared(SymbolId),
    /// Already defined. This is an error.
    AlreadyDefined(SymbolId),
}

impl<'a> SemanticCheckerVisitor<'a> {
    fn _lookup_symbol_impl(
        &mut self,
        name: &str,
        span: &span::SpanLoc,
        diagnose: bool,
    ) -> Option<SymbolId> {
        let query = self.ctx.scope.lookup(name);
        if query.is_none() {
            if diagnose {
                self.diagnostics.add(
                    DiagnosticKind::Error,
                    *span,
                    format!("identifier '{}' not found in this scope", name),
                );
            }
            // Now create an erroneous identifier that we will use to filter later occurrences.
            let mut dummy_sym = Symbol::new();
            dummy_sym.set_name(name);
            dummy_sym.set_kind(SymbolKind::ErrorLookup);
            dummy_sym.set_defining_point(*span);
            dummy_sym.set_scope(self.ctx.scope.get_current_scope_id());

            let dummy_sym = self.ctx.new_symbol(dummy_sym);

            self.ctx.scope.add_entry(name, dummy_sym);
        }
        query
    }

    fn lookup_symbol(&mut self, name: &str, span: &span::SpanLoc) -> Option<SymbolId> {
        self._lookup_symbol_impl(name, span, /* diagnose */ true)
    }

    fn lookup_symbol_and_diagnose(
        &mut self,
        name: &str,
        span: &span::SpanLoc,
        diagnose: bool,
    ) -> Option<SymbolId> {
        self._lookup_symbol_impl(name, span, diagnose)
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
        let sym = self.ctx.scope.lookup_current_scope(name);
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

    fn is_pending_type_definition(&mut self, name: &String) -> Option<SymbolId> {
        let sym = self.ctx.scope.lookup_current_scope(name);
        if let Some(sym_id) = sym {
            let sym = self.ctx.get_symbol(sym_id);
            let sym = sym.borrow();
            if sym.get_kind() != SymbolKind::PendingTypeDefinition {
                return None;
            }
        }

        sym
    }

    fn diagnose_reintroduced_procedure_or_function(
        &mut self,
        name: &String,
        span: &span::SpanLoc,
        is_procedure: bool,
    ) -> FunctionProcedureDeclarationStatus {
        if let Some(sym_id) = self.ctx.scope.lookup_current_scope(&name) {
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
                        return FunctionProcedureDeclarationStatus::AlreadyDefined(sym_id);
                    } else {
                        return FunctionProcedureDeclarationStatus::ForwardDeclared(sym_id);
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
                        return FunctionProcedureDeclarationStatus::AlreadyDefined(sym_id);
                    } else {
                        return FunctionProcedureDeclarationStatus::ForwardDeclared(sym_id);
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
                    return FunctionProcedureDeclarationStatus::AlreadyDeclared(sym_id);
                }
            }
        }
        FunctionProcedureDeclarationStatus::NotDeclared
    }

    fn diagnose_reintroduced_procedure_definition(
        &mut self,
        name: &String,
        span: &span::SpanLoc,
    ) -> FunctionProcedureDeclarationStatus {
        self.diagnose_reintroduced_procedure_or_function(name, span, true)
    }

    fn diagnose_reintroduced_procedure_declaration(
        &mut self,
        name: &String,
        span: &span::SpanLoc,
    ) -> FunctionProcedureDeclarationStatus {
        self.diagnose_reintroduced_procedure_or_function(name, span, true)
    }

    fn diagnose_reintroduced_function_definition(
        &mut self,
        name: &String,
        span: &span::SpanLoc,
    ) -> FunctionProcedureDeclarationStatus {
        self.diagnose_reintroduced_procedure_or_function(name, span, false)
    }

    fn diagnose_reintroduced_function_declaration(
        &mut self,
        name: &String,
        span: &span::SpanLoc,
    ) -> FunctionProcedureDeclarationStatus {
        self.diagnose_reintroduced_procedure_or_function(name, span, false)
    }

    fn gather_formal_parameters(
        &mut self,
        params: &mut Option<Vec<span::SpannedBox<ast::FormalParameter>>>,
    ) -> Option<Vec<Vec<SymbolId>>> {
        let mut formal_parameters: Vec<Vec<SymbolId>> = vec![];
        if let Some(parameters) = params {
            parameters.iter_mut().for_each(|param| {
                let loc = *param.loc();
                let id = param.id();
                param.get_mut().mutating_walk_mut(self, &loc, id);

                // let mut register_parameter = |name: &span::Spanned<String>| {
                //     formal_parameters.push(self.ctx.get_ast_symbol(name.id()).unwrap());
                // };

                let get_symbol =
                    |name: &span::Spanned<String>| self.ctx.get_ast_symbol(name.id()).unwrap();

                match param.get() {
                    FormalParameter::Value(n) => {
                        formal_parameters.push(n.0.iter().map(get_symbol).collect());
                    }
                    FormalParameter::Variable(n) => {
                        formal_parameters.push(n.0.iter().map(get_symbol).collect());
                    }
                    FormalParameter::Function(n) => {
                        formal_parameters.push(vec![get_symbol(&n.0)]);
                    }
                    FormalParameter::Procedure(n) => {
                        formal_parameters.push(vec![get_symbol(&n.0)]);
                    }
                    FormalParameter::ValueConformableArray(n) => {
                        formal_parameters.push(n.0.iter().map(get_symbol).collect());
                    }
                    FormalParameter::VariableConformableArray(n) => {
                        formal_parameters.push(n.0.iter().map(get_symbol).collect());
                    }
                };
            });
            Some(formal_parameters)
        } else {
            None
        }
    }

    fn gather_return_type(
        &mut self,
        return_type: &mut span::SpannedBox<ast::TypeIdentifier>,
    ) -> TypeId {
        let loc = *return_type.loc();
        let id = return_type.id();
        return_type.get_mut().mutating_walk_mut(self, &loc, id);
        self.ctx.get_ast_type(return_type.id()).unwrap()
    }

    fn compare_parameter_declarations(
        &self,
        params_a: &Vec<Vec<SymbolId>>,
        params_b: &Vec<Vec<SymbolId>>,
    ) -> bool {
        if params_a.iter().flatten().count() != params_b.iter().flatten().count() {
            return false;
        }

        for p in params_a.iter().flatten().zip(params_b.iter().flatten()) {
            let sym_a = self.ctx.get_symbol(*p.0);
            let sym_a = sym_a.borrow();

            let sym_b = self.ctx.get_symbol(*p.1);
            let sym_b = sym_b.borrow();

            let param_kind_a = sym_a.get_parameter().unwrap();
            let param_kind_b = sym_b.get_parameter().unwrap();
            match (param_kind_a, param_kind_b) {
                (ParameterKind::Value, ParameterKind::Value)
                | (ParameterKind::Variable, ParameterKind::Variable) => {}
                _ => {
                    return false;
                }
            }

            let type_a = sym_a.get_type().unwrap();
            let type_b = sym_b.get_type().unwrap();

            if !self.ctx.type_system.same_type(type_a, type_b) {
                return false;
            }
        }

        true
    }

    fn equivalent_function_declarations(
        &self,
        prev_sym_id: SymbolId,
        formal_parameters: &Vec<Vec<SymbolId>>,
        result_type: TypeId,
    ) -> bool {
        let prev_sym = self.ctx.get_symbol(prev_sym_id);
        let prev_sym = prev_sym.borrow();

        let prev_params = prev_sym.get_formal_parameters().unwrap();

        let prev_result_sym_id = prev_sym.get_return_symbol().unwrap();
        let prev_result_sym = self.ctx.get_symbol(prev_result_sym_id);
        let prev_result_sym = prev_result_sym.borrow();
        let prev_result_type = prev_result_sym.get_type().unwrap();

        self.compare_parameter_declarations(&prev_params, formal_parameters)
            && self
                .ctx
                .type_system
                .same_type(result_type, prev_result_type)
    }

    fn equivalent_function_symbols(&self, sym_id_1: SymbolId, sym_id_2: SymbolId) -> bool {
        let sym_1 = self.ctx.get_symbol(sym_id_1);
        let sym_1 = sym_1.borrow();
        let sym_1_return_type = {
            let result_sym_id = sym_1.get_return_symbol().unwrap();
            let result_sym = self.ctx.get_symbol(result_sym_id);
            let result_sym = result_sym.borrow();
            result_sym.get_type().unwrap()
        };
        self.equivalent_function_declarations(
            sym_id_2,
            sym_1.get_formal_parameters().as_ref().unwrap(),
            sym_1_return_type,
        )
    }

    fn create_new_function_symbol(
        &mut self,
        scope_id: scope::ScopeId,
        function_name: &str,
        formal_parameters: Vec<Vec<SymbolId>>,
        result_type: TypeId,
        is_definition: bool,
        defining_loc: span::SpanLoc,
    ) -> SymbolId {
        let mut function_sym = Symbol::new();
        function_sym.set_name(function_name);
        function_sym.set_kind(SymbolKind::Function);
        function_sym.set_defining_point(defining_loc);
        function_sym.set_defined(is_definition);
        function_sym.set_scope(scope_id);
        function_sym.set_formal_parameters(formal_parameters);

        let function_sym_id = self.ctx.new_symbol(function_sym);
        self.ctx
            .scope
            .add_entry_to_scope(scope_id, function_name, function_sym_id);

        let mut return_symbol = Symbol::new();
        return_symbol.set_name(function_name);
        return_symbol.set_kind(SymbolKind::Variable);
        return_symbol.set_defining_point(defining_loc);
        return_symbol.set_type(result_type);

        let return_symbol_id = self.ctx.new_symbol(return_symbol);

        self.ctx
            .get_symbol_mut(function_sym_id)
            .borrow_mut()
            .set_return_symbol(return_symbol_id);

        function_sym_id
    }

    fn equivalent_procedure_declarations(
        &self,
        prev_sym_id: SymbolId,
        formal_parameters: &Vec<Vec<SymbolId>>,
    ) -> bool {
        let prev_sym = self.ctx.get_symbol(prev_sym_id);
        let prev_sym = prev_sym.borrow();

        let prev_params = prev_sym.get_formal_parameters().unwrap();

        self.compare_parameter_declarations(&prev_params, formal_parameters)
    }

    fn equivalent_procedure_symbols(&self, sym_id_1: SymbolId, sym_id_2: SymbolId) -> bool {
        let sym_1 = self.ctx.get_symbol(sym_id_1);
        let sym_1 = sym_1.borrow();
        self.equivalent_procedure_declarations(
            sym_id_2,
            sym_1.get_formal_parameters().as_ref().unwrap(),
        )
    }

    fn create_new_procedure_symbol(
        &mut self,
        scope_id: scope::ScopeId,
        proc_name: &str,
        formal_parameters: Vec<Vec<SymbolId>>,
        is_definition: bool,
        defining_loc: span::SpanLoc,
    ) -> SymbolId {
        let mut proc_sym = Symbol::new();
        proc_sym.set_name(proc_name);
        proc_sym.set_kind(SymbolKind::Procedure);
        proc_sym.set_defining_point(defining_loc);
        proc_sym.set_defined(is_definition);
        proc_sym.set_scope(scope_id);
        proc_sym.set_formal_parameters(formal_parameters);

        let proc_sym_id = self.ctx.new_symbol(proc_sym);
        self.ctx
            .scope
            .add_entry_to_scope(scope_id, proc_name, proc_sym_id);

        proc_sym_id
    }

    fn is_pointer_and_generic_pointer(&self, a: TypeId, b: TypeId) -> bool {
        self.ctx.type_system.is_pointer_type(a) && self.ctx.type_system.is_generic_pointer_type(b)
    }

    fn is_set_and_generic_set(&self, a: TypeId, b: TypeId) -> bool {
        self.ctx.type_system.is_set_type(a) && self.ctx.type_system.is_generic_set_type(b)
    }

    fn is_compatible(&self, lhs_type_id: TypeId, rhs_type_id: TypeId) -> bool {
        // Two generic pointer types are assumed incompatible. This can only happen if the code does `nil <> nil`.
        // FIXME: we could allow this case but it is rather useless so little is lost by not allowing it.
        if self.ctx.type_system.is_generic_pointer_type(lhs_type_id)
            && self.ctx.type_system.is_generic_pointer_type(rhs_type_id)
        {
            return false;
        }

        // Two generic set types are assumed incompatible. This can only happen if the code does `[] <> []`.
        // FIXME: we could allow this case but it is rather useless so little is lost by not allowing it.
        if self.ctx.type_system.is_generic_set_type(lhs_type_id)
            && self.ctx.type_system.is_generic_set_type(rhs_type_id)
        {
            return false;
        }

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

        // lhs and rhs are set-types of compatible base-types, and either both lhs and rhs are designated packed or neither lhs nor rhs is designated packed.
        if self.ctx.type_system.is_set_type(lhs_type_id)
            && self.ctx.type_system.is_set_type(rhs_type_id)
            && self.is_compatible(
                self.ctx.type_system.set_type_get_element(lhs_type_id),
                self.ctx.type_system.set_type_get_element(rhs_type_id),
            )
        {
            return match (
                self.ctx.type_system.set_type_get_packed(lhs_type_id),
                self.ctx.type_system.set_type_get_packed(rhs_type_id),
            ) {
                (Some(true), Some(true)) | (Some(false), Some(false)) | (None, _) | (_, None) => {
                    true
                }
                _ => false,
            };
        }

        // We have to allow operations of the form `nil <> p` and `p <> nil`.
        // Something like `nil <> nil` has been handled earlier.
        if self.is_pointer_and_generic_pointer(rhs_type_id, lhs_type_id)
            || self.is_pointer_and_generic_pointer(lhs_type_id, rhs_type_id)
        {
            return true;
        }

        // We have to allow operations of the form `[] <> s` and `s <> []`.
        // Something like `[] <> []` has been handled earlier.
        if self.is_set_and_generic_set(rhs_type_id, lhs_type_id)
            || self.is_set_and_generic_set(lhs_type_id, rhs_type_id)
        {
            return true;
        }

        false
    }

    fn is_assignment_compatible(&self, lhs_type_id: TypeId, rhs_type_id: TypeId) -> bool {
        // Two generic pointers are assumed incompatible (this should not happen)
        if self.ctx.type_system.is_generic_pointer_type(lhs_type_id)
            && self.ctx.type_system.is_generic_pointer_type(rhs_type_id)
        {
            return false;
        }

        // Two generic sets are assumed incompatible (this should not happen)
        if self.ctx.type_system.is_generic_set_type(lhs_type_id)
            && self.ctx.type_system.is_generic_set_type(rhs_type_id)
        {
            return false;
        }

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

        // types are compatible set types and all the members of rhs are in the closed interval specified by lhs
        if self.ctx.type_system.is_set_type(lhs_type_id)
            && self.ctx.type_system.is_set_type(rhs_type_id)
            && self.is_compatible(lhs_type_id, rhs_type_id)
        {
            // FIXME: There are cases that we might be able to diagnose here statically.
            return true;
        }

        // p := nil;
        if self.is_pointer_and_generic_pointer(lhs_type_id, rhs_type_id) {
            return true;
        }

        // s := [];
        if self.is_set_and_generic_set(lhs_type_id, rhs_type_id) {
            return true;
        }

        false
    }

    fn type_is_array_or_conformable_array(&self, ty: TypeId) -> bool {
        self.ctx.type_system.is_array_type(ty) || self.ctx.type_system.is_conformable_array_type(ty)
    }

    fn type_can_be_conformed_to(&self, source_type: TypeId, target_type: TypeId) -> bool {
        // Base type.
        if !self.type_is_array_or_conformable_array(source_type)
            && !self.type_is_array_or_conformable_array(target_type)
        {
            return self.ctx.type_system.same_type(source_type, target_type);
        }
        // Neither is a conformable array.
        if self.ctx.type_system.is_array_type(source_type)
            && self.ctx.type_system.is_array_type(target_type)
        {
            return self.ctx.type_system.same_type(source_type, target_type);
        }

        if self.ctx.type_system.is_array_type(source_type)
            && self.ctx.type_system.is_conformable_array_type(target_type)
        {
            // FIXME: We should check that the ranges of the indices are compatible.
            let source_component = self
                .ctx
                .type_system
                .array_type_get_component_type(source_type);
            let target_component = self
                .ctx
                .type_system
                .conformable_array_type_get_component_type(target_type);
            return self.type_can_be_conformed_to(source_component, target_component);
        } else if self.ctx.type_system.is_conformable_array_type(source_type)
            && self.ctx.type_system.is_conformable_array_type(target_type)
        {
            // FIXME: We should check that the ranges of the indices are compatible.
            let source_component = self
                .ctx
                .type_system
                .conformable_array_type_get_component_type(source_type);
            let target_component = self
                .ctx
                .type_system
                .conformable_array_type_get_component_type(target_type);
            return self.type_can_be_conformed_to(source_component, target_component);
        }

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

    fn same_set_type(&self, lhs_ty: TypeId, rhs_ty: TypeId) -> Option<TypeId> {
        // Special cases when operating with []
        if self.is_set_and_generic_set(lhs_ty, rhs_ty) {
            return Some(lhs_ty);
        }

        if self.is_set_and_generic_set(rhs_ty, lhs_ty) {
            return Some(rhs_ty);
        }

        if !self.ctx.type_system.is_set_type(lhs_ty) || !self.ctx.type_system.is_set_type(rhs_ty) {
            return None;
        }

        if !self.ctx.type_system.same_type(
            self.ctx.type_system.set_type_get_element(lhs_ty),
            self.ctx.type_system.set_type_get_element(rhs_ty),
        ) {
            return None;
        }

        match (
            self.ctx.type_system.set_type_get_packed(lhs_ty),
            self.ctx.type_system.set_type_get_packed(rhs_ty),
        ) {
            (Some(true), Some(true)) | (Some(false), Some(false)) | (_, None) | (None, _) => {
                Some(rhs_ty)
            }
            _ => None,
        }
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

    // <
    // >
    fn valid_for_relational_strict(
        &self,
        lhs_type_id: TypeId,
        rhs_type_id: TypeId,
    ) -> Option<TypeId> {
        let valid_type = |ty: TypeId| {
            self.ctx.type_system.is_simple_type(ty) // || self.ctx.type_system.is_string_type(ty))
        };

        if !valid_type(lhs_type_id) || !valid_type(rhs_type_id) {
            return None;
        }

        self.relational_compatibility(lhs_type_id, rhs_type_id)
    }

    // <=
    // >=
    fn valid_for_relational_nonstrict(
        &self,
        lhs_type_id: TypeId,
        rhs_type_id: TypeId,
    ) -> Option<TypeId> {
        let valid_type = |ty: TypeId| {
            self.ctx.type_system.is_simple_type(ty)
                || self.ctx.type_system.is_set_type(ty)
                || self.ctx.type_system.is_generic_set_type(ty)
            // || self.ctx.type_system.is_string_type(ty))
        };

        if !valid_type(lhs_type_id) || !valid_type(rhs_type_id) {
            return None;
        }

        self.relational_compatibility(lhs_type_id, rhs_type_id)
    }

    // =
    // <>
    fn valid_for_equality(&self, lhs_type_id: TypeId, rhs_type_id: TypeId) -> Option<TypeId> {
        let valid_type = |ty: TypeId| {
            self.ctx.type_system.is_simple_type(ty)
                || self.ctx.type_system.is_set_type(ty)
                || self.ctx.type_system.is_generic_set_type(ty)
                || self.ctx.type_system.is_pointer_type(ty)
                || self.ctx.type_system.is_generic_pointer_type(ty)
            // self.ctx.type_system.is_string_type(ty)
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
        new_sym.set_scope(self.ctx.scope.get_current_scope_id());

        let new_sym_id = self.ctx.new_symbol(new_sym);
        self.ctx.scope.add_entry(&name.get().clone(), new_sym_id);

        self.ctx.set_ast_symbol(name.id(), new_sym_id);
    }

    fn declare_function_parameter(
        &mut self,
        name: &span::Spanned<String>,
        formal_parameters: Vec<Vec<SymbolId>>,
        result_type: TypeId,
    ) {
        let mut function_sym = Symbol::new();
        function_sym.set_name(name.get());
        function_sym.set_kind(SymbolKind::Function);
        function_sym.set_defining_point(*name.loc());
        function_sym.set_scope(self.ctx.scope.get_current_scope_id());
        function_sym.set_parameter(ParameterKind::Function);
        function_sym.set_formal_parameters(formal_parameters);

        let function_sym_id = self.ctx.new_symbol(function_sym);
        self.ctx.scope.add_entry_to_scope(
            self.ctx.scope.get_current_scope_id(),
            name.get(),
            function_sym_id,
        );

        let mut return_symbol = Symbol::new();
        return_symbol.set_name(name.get());
        return_symbol.set_kind(SymbolKind::Variable);
        return_symbol.set_defining_point(*name.loc());
        return_symbol.set_type(result_type);

        let return_symbol_id = self.ctx.new_symbol(return_symbol);

        self.ctx
            .get_symbol_mut(function_sym_id)
            .borrow_mut()
            .set_return_symbol(return_symbol_id);

        self.ctx.set_ast_symbol(name.id(), function_sym_id);
    }

    fn declare_procedure_parameter(
        &mut self,
        name: &span::Spanned<String>,
        formal_parameters: Vec<Vec<SymbolId>>,
    ) {
        let mut proc_sym = Symbol::new();
        proc_sym.set_name(name.get());
        proc_sym.set_kind(SymbolKind::Procedure);
        proc_sym.set_defining_point(*name.loc());
        proc_sym.set_scope(self.ctx.scope.get_current_scope_id());
        proc_sym.set_parameter(ParameterKind::Procedure);
        proc_sym.set_formal_parameters(formal_parameters);

        let proc_sym_id = self.ctx.new_symbol(proc_sym);
        self.ctx.scope.add_entry_to_scope(
            self.ctx.scope.get_current_scope_id(),
            name.get(),
            proc_sym_id,
        );

        self.ctx.set_ast_symbol(name.id(), proc_sym_id);
    }

    fn get_symbols_formal_parameter(
        &self,
        formal_param: &SpannedBox<ast::FormalParameter>,
    ) -> Vec<SymbolId> {
        match formal_param.get() {
            ast::FormalParameter::Value(param) => param
                .0
                .iter()
                .map(|x| self.ctx.get_ast_symbol(x.id()).unwrap())
                .collect(),
            ast::FormalParameter::Variable(param) => param
                .0
                .iter()
                .map(|x| self.ctx.get_ast_symbol(x.id()).unwrap())
                .collect(),
            ast::FormalParameter::Function(param, ..) => {
                vec![self.ctx.get_ast_symbol(param.0.id()).unwrap()]
            }
            ast::FormalParameter::Procedure(param, ..) => {
                vec![self.ctx.get_ast_symbol(param.0.id()).unwrap()]
            }
            ast::FormalParameter::ValueConformableArray(param) => param
                .0
                .iter()
                .map(|x| self.ctx.get_ast_symbol(x.id()).unwrap())
                .collect(),
            ast::FormalParameter::VariableConformableArray(param) => param
                .0
                .iter()
                .map(|x| self.ctx.get_ast_symbol(x.id()).unwrap())
                .collect(),
        }
    }

    fn common_check_parameters(
        &mut self,
        is_function: bool,
        function_name: &str,
        args: &mut Vec<span::SpannedBox<ast::Expr>>,
        call_loc: span::SpanLoc,
        callee_symbol_id: SymbolId,
    ) -> bool {
        let params = {
            let callee_symbol = self.ctx.get_symbol(callee_symbol_id);
            let callee_symbol = callee_symbol.borrow();
            callee_symbol.get_formal_parameters().unwrap()
        };

        let num_params = params.iter().flatten().count();
        let num_args = args.len();

        if num_args != num_params {
            self.diagnostics.add(
                DiagnosticKind::Error,
                call_loc,
                format!(
                    "{} '{}' expects {} {} but {} {} passed",
                    if is_function { "function" } else { "procedure" },
                    function_name,
                    num_params,
                    if num_params == 1 {
                        "parameter"
                    } else {
                        "parameters"
                    },
                    num_args,
                    if num_args == 1 {
                        "argument was"
                    } else {
                        "arguments were"
                    },
                ),
            );
            return false;
        }

        let params_flattened: Vec<_> = params
            .iter()
            .enumerate()
            .map(|(param_pack_idx, param_pack)| {
                param_pack.iter().map(move |param| (param_pack_idx, *param))
            })
            .flatten()
            .collect();

        assert!(args.len() == params_flattened.len());

        let mut argument_error = false;
        let mut check_argument_consistency: HashMap<usize, Vec<usize>> = HashMap::new();
        for ((current_arg_idx, arg), (current_param_pack_idx, param_sym_id)) in
            args.iter_mut().enumerate().zip(params_flattened)
        {
            let param_sym = self.ctx.get_symbol(param_sym_id);
            let param_sym = param_sym.borrow();
            let param_kind = param_sym.get_parameter().unwrap();
            match param_kind {
                ParameterKind::Value => {
                    let param_type_id = param_sym.get_type().unwrap();
                    if self.ctx.type_system.is_error_type(param_type_id) {
                        continue;
                    }
                    // Typecheck argument now.
                    {
                        let loc = *arg.loc();
                        let id = arg.id();
                        arg.get_mut().mutating_walk_mut(self, &loc, id);
                    }
                    let arg_type_id = self.ctx.get_ast_type(arg.id()).unwrap();
                    if self.ctx.type_system.is_error_type(arg_type_id) {
                        argument_error = true;
                        continue;
                    }
                    if !self.is_assignment_compatible(param_type_id, arg_type_id) {
                        self.diagnostics.add(DiagnosticKind::Error,
                                                *arg.loc(),
                                                format!(
                                                    "argument has type {} that is not assignment compatible with value parameter '{}' of type {}",
                                                    self.ctx.type_system.get_type_name(arg_type_id),
                                                    param_sym.get_name(),
                                                    self.ctx.type_system.get_type_name(param_type_id)
                                                ),
                                            );
                        argument_error = true;
                    } else if self.second_needs_conversion_to_first(param_type_id, arg_type_id) {
                        let conversion = SemanticCheckerVisitor::create_conversion_expr(arg.take());
                        arg.reset(conversion);
                        self.ctx.set_ast_type(arg.id(), param_type_id);
                    } else if self.is_pointer_and_generic_pointer(param_type_id, arg_type_id)
                        || self.is_set_and_generic_set(param_type_id, arg_type_id)
                    {
                        self.ctx.set_ast_type(arg.id(), param_type_id);
                    }
                }
                ParameterKind::Variable => {
                    let param_type_id = param_sym.get_type().unwrap();
                    if self.ctx.type_system.is_error_type(param_type_id) {
                        continue;
                    }
                    // Typecheck argument now.
                    {
                        let loc = *arg.loc();
                        let id = arg.id();
                        arg.get_mut().mutating_walk_mut(self, &loc, id);
                    }
                    let arg_type_id = self.ctx.get_ast_type(arg.id()).unwrap();
                    if self.ctx.type_system.is_error_type(arg_type_id) {
                        argument_error = true;
                        continue;
                    }
                    let arg_sym = self.ctx.get_ast_symbol(arg.id());

                    let expr_is_variable = match arg.get() {
                        ast::Expr::Variable(_) => true,
                        _ => false,
                    };

                    if arg_sym.is_none() || !expr_is_variable {
                        self.diagnostics.add(
                                                DiagnosticKind::Error,
                                                *arg.loc(),
                                                format!(
                                                    "argument is not a variable, as required by variable parameter '{}'",
                                                    param_sym.get_name()
                                                ),
                                            );
                        argument_error = true;
                    } else if !self.ctx.type_system.same_type(param_type_id, arg_type_id) {
                        self.diagnostics.add(
                                                DiagnosticKind::Error,
                                                *arg.loc(),
                                                format!(
                                                    "argument has type {} but it is different to variable parameter '{}' of type {}",
                                                    self.ctx.type_system.get_type_name(arg_type_id),
                                                    param_sym.get_name(),
                                                    self.ctx.type_system.get_type_name(param_type_id)
                                                ),
                                            );
                        argument_error = true;
                    } else {
                        assert!(expr_is_variable);
                        let argument = arg.get_mut();
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
                ParameterKind::Function | ParameterKind::Procedure => {
                    let param_kind_name = if param_kind == ParameterKind::Function {
                        "function"
                    } else {
                        "procedure"
                    };
                    let mut valid_argument = false;
                    let mut valid_name = false;
                    match arg.get() {
                        ast::Expr::Variable(expr_var) => match expr_var.0.get() {
                            ast::Assig::Variable(var) => {
                                let name = var.0.get();
                                if let Some(arg_sym_id) = self.lookup_symbol(name, var.0.loc()) {
                                    let arg_sym = self.ctx.get_symbol(arg_sym_id);
                                    let arg_sym = arg_sym.borrow();
                                    match (param_kind, arg_sym.get_kind()) {
                                        (ParameterKind::Function, SymbolKind::Function)
                                        | (ParameterKind::Procedure, SymbolKind::Procedure) => {
                                            valid_name = true;

                                            let equivalent_decl =
                                                if param_kind == ParameterKind::Function {
                                                    self.equivalent_function_symbols(
                                                        param_sym_id,
                                                        arg_sym_id,
                                                    )
                                                } else {
                                                    self.equivalent_procedure_symbols(
                                                        param_sym_id,
                                                        arg_sym_id,
                                                    )
                                                };

                                            if !equivalent_decl {
                                                self.diagnostics.add_with_extra(DiagnosticKind::Error, *arg.loc(),
                                                   format!("{param_kind_name} argument is not compatible with the {param_kind_name} parameter"),
                                                   vec![],
                                                   vec![Diagnostic::new(DiagnosticKind::Error,
                                                    {
                                                      let param_sym = self.ctx.get_symbol(param_sym_id);
                                                      let param_sym = param_sym.borrow();
                                                      param_sym.get_defining_point().unwrap()
                                                    }, format!("declaration of the {} parameter", param_kind_name))],);
                                            } else {
                                                self.ctx
                                                    .set_ast_symbol(expr_var.0.id(), arg_sym_id);
                                                valid_argument = true;
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            _ => {}
                        },
                        _ => {}
                    }
                    if !valid_argument {
                        argument_error = true;
                        if !valid_name {
                            self.diagnostics.add(
                                DiagnosticKind::Error,
                                *arg.loc(),
                                format!("argument should be the name of a {}", param_kind_name),
                            );
                        }
                        continue;
                    } else {
                        let argument = arg.get_mut();
                        match argument {
                            ast::Expr::Variable(expr_variable) => {
                                let assig = &mut expr_variable.0;
                                let assig = assig.take();
                                let func_reference = ast::Expr::VariableReference(
                                    ExprVariableReference(span::SpannedBox::from(assig)),
                                );
                                *argument = func_reference;
                            }
                            _ => {
                                panic!("Unexpected tree");
                            }
                        }
                    }
                }
                ParameterKind::ValueConformableArray => {
                    let param_type_id = param_sym.get_type().unwrap();
                    if self.ctx.type_system.is_error_type(param_type_id) {
                        continue;
                    }
                    // Typecheck argument now.
                    {
                        let loc = *arg.loc();
                        let id = arg.id();
                        arg.get_mut().mutating_walk_mut(self, &loc, id);
                    }
                    let arg_type_id = self.ctx.get_ast_type(arg.id()).unwrap();
                    if self.ctx.type_system.is_error_type(arg_type_id) {
                        argument_error = true;
                        continue;
                    }

                    if self.ctx.type_system.is_conformable_array_type(arg_type_id) {
                        self.diagnostics.add(
                            DiagnosticKind::Error,
                            *arg.loc(),
                            format!(
                                "a conformable array cannot be passed to the value conformable array parameter {}",
                                param_sym.get_name()
                            ),
                        );
                        argument_error = true;
                        continue;
                    } else if !self.type_can_be_conformed_to(arg_type_id, param_type_id) {
                        self.diagnostics.add(
                            DiagnosticKind::Error,
                            *arg.loc(),
                            format!(
                                "argument is not conformable to the parameter {}",
                                param_sym.get_name()
                            ),
                        );
                        argument_error = true;
                        continue;
                    }

                    check_argument_consistency
                        .entry(current_param_pack_idx)
                        .and_modify(|x| x.push(current_arg_idx))
                        .or_insert(vec![current_arg_idx]);
                }
                ParameterKind::VariableConformableArray => {
                    let param_type_id = param_sym.get_type().unwrap();
                    if self.ctx.type_system.is_error_type(param_type_id) {
                        continue;
                    }
                    // Typecheck argument now.
                    {
                        let loc = *arg.loc();
                        let id = arg.id();
                        arg.get_mut().mutating_walk_mut(self, &loc, id);
                    }
                    let arg_type_id = self.ctx.get_ast_type(arg.id()).unwrap();
                    if self.ctx.type_system.is_error_type(arg_type_id) {
                        argument_error = true;
                        continue;
                    }

                    let arg_sym = self.ctx.get_ast_symbol(arg.id());
                    let expr_is_variable = match arg.get() {
                        ast::Expr::Variable(_) => true,
                        _ => false,
                    };

                    if arg_sym.is_none() || !expr_is_variable {
                        self.diagnostics.add(
                                                DiagnosticKind::Error,
                                                *arg.loc(),
                                                format!(
                                                    "argument is not a variable, as required by variable conformable array parameter '{}'",
                                                    param_sym.get_name()
                                                ),
                                            );
                        argument_error = true;
                        continue;
                    } else if !self.type_can_be_conformed_to(arg_type_id, param_type_id) {
                        self.diagnostics.add(
                            DiagnosticKind::Error,
                            *arg.loc(),
                            format!(
                                "argument is not conformable to the parameter {}",
                                param_sym.get_name()
                            ),
                        );
                        argument_error = true;
                        continue;
                    } else {
                        assert!(expr_is_variable);
                        let argument = arg.get_mut();
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

                    check_argument_consistency
                        .entry(current_param_pack_idx)
                        .and_modify(|x| x.push(current_arg_idx))
                        .or_insert(vec![current_arg_idx]);
                }
            }
        }

        if !argument_error {
            // Now check the consistency of the arguments.
            for (_param_pack_idx, args_to_check) in check_argument_consistency {
                // Compare first with the rest.
                let first_arg = &args[args_to_check[0]];
                let first_arg_type_id = self.ctx.get_ast_type(first_arg.id()).unwrap();

                for i in 1..args_to_check.len() {
                    let current_arg = &args[args_to_check[i]];
                    let current_arg_type_id = self.ctx.get_ast_type(current_arg.id()).unwrap();
                    if !self
                        .ctx
                        .type_system
                        .same_type(first_arg_type_id, current_arg_type_id)
                    {
                        self.diagnostics.add_with_extra(
                            DiagnosticKind::Error,
                            *current_arg.loc(),
                            format!("these arguments should have the same type",),
                            vec![*first_arg.loc()],
                            vec![],
                        );
                        argument_error = true;
                    }
                }
            }
        }

        argument_error
    }

    fn contains_invalid_type_cycle_impl(
        &self,
        top_level: bool,
        root_ty: TypeId,
        ty: TypeId,
    ) -> bool {
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
            let fields = self.ctx.type_system.record_type_get_all_fields(ty);
            for field in fields {
                let field_sym = self.ctx.get_symbol(*field);
                let field_sym = field_sym.borrow();
                if self.contains_invalid_type_cycle_impl(
                    false,
                    root_ty,
                    field_sym.get_type().unwrap(),
                ) {
                    return true;
                }
            }
        }
        false
    }

    fn contains_invalid_type_cycle(&self, root_ty: TypeId) -> bool {
        self.contains_invalid_type_cycle_impl(true, root_ty, root_ty)
    }

    fn _ensure_global_file(&mut self, name: &str, span: &span::SpanLoc) {
        let query = self.ctx.scope.lookup_program_scope(name);
        if query.is_none() {
            self.diagnostics.add_with_extra(
                DiagnosticKind::Error,
                *span,
                format!(
                    "program parameters should mention the required file variable '{}'",
                    name
                ),
                vec![],
                vec![Diagnostic::new(
                    DiagnosticKind::Info,
                    self.program_heading_loc.unwrap(),
                    format!("'{}' should be mentioned here", name),
                )],
            );
            let mut new_sym = Symbol::new();
            new_sym.set_name(name);
            new_sym.set_kind(SymbolKind::Variable);
            new_sym.set_type(self.ctx.type_system.get_textfile_type());
            new_sym.set_defining_point(*span);

            let new_sym = self.ctx.new_symbol(new_sym);
            self.ctx.scope.add_entry_program_scope(name, new_sym);
        }
    }

    fn ensure_input(&mut self, span: &span::SpanLoc) {
        self._ensure_global_file("input", span);
    }

    fn ensure_output(&mut self, span: &span::SpanLoc) {
        self._ensure_global_file("output", span);
    }

    fn analyze_write_read_args(
        &mut self,
        procedure_name: &str,
        global_file: &str,
        is_newline_version: bool,
        span: &span::SpanLoc,
        args: &Vec<span::SpannedBox<ast::Expr>>,
    ) -> (usize, TypeId, bool) {
        let is_textfile;
        let file_component: TypeId;
        let mut first_arg = 0;
        if !is_newline_version && args.is_empty() {
            // This is unlikelyy to happen.
            self.diagnostics.add(
                DiagnosticKind::Error,
                *span,
                format!("too few arguments in call to {}", procedure_name),
            );
            file_component = self.ctx.type_system.get_error_type();
            is_textfile = false;
        } else if !is_newline_version {
            let file_arg = &args[0];
            let ty = self.ctx.get_ast_type(file_arg.id()).unwrap();
            if self.ctx.type_system.is_file_type(ty) {
                if self.ctx.type_system.is_textfile_type(ty) {
                    is_textfile = true;
                } else {
                    is_textfile = false;
                }
                if args.len() < 2 {
                    // write(file)
                    // read(file)
                    self.diagnostics.add(
                        DiagnosticKind::Error,
                        *span,
                        format!("too few arguments in call to {}", procedure_name),
                    );
                }
                file_component = self.ctx.type_system.file_type_get_component_type(ty);
                first_arg = 1;
            } else if self.ctx.type_system.is_error_type(ty) {
                file_component = self.ctx.type_system.get_error_type();
                is_textfile = false;
            } else {
                self._ensure_global_file(global_file, span);
                file_component = self.ctx.type_system.get_char_type();
                is_textfile = true;
            }
        } else {
            assert!(is_newline_version);
            if args.len() > 0 {
                // Check if the first argument is a textfile.
                let file_arg = &args[0];
                let ty = self.ctx.get_ast_type(file_arg.id()).unwrap();
                if self.ctx.type_system.is_file_type(ty) {
                    if !self.ctx.type_system.is_textfile_type(ty) {
                        self.diagnostics.add(
                            DiagnosticKind::Error,
                            *file_arg.loc(),
                            format!("{}ln can only be applied to textfiles", procedure_name),
                        );
                    }
                    is_textfile = true;
                    file_component = self.ctx.type_system.get_char_type();
                    first_arg = 1;
                } else if self.ctx.type_system.is_error_type(ty) {
                    file_component = self.ctx.type_system.get_char_type();
                    is_textfile = true;
                } else {
                    self._ensure_global_file(global_file, span);
                    file_component = self.ctx.type_system.get_char_type();
                    is_textfile = true;
                }
            } else {
                self._ensure_global_file(global_file, span);
                file_component = self.ctx.type_system.get_char_type();
                is_textfile = true;
            }
        }
        (first_arg, file_component, is_textfile)
    }

    fn record_type_get_all_fields_of_variant(
        &self,
        variant_part: &Option<typesystem::VariantPart>,
        result: &mut Vec<SymbolId>,
    ) {
        if let Some(variant_part) = variant_part {
            result.push(variant_part.tag_name);
            variant_part.cases.iter().for_each(|case| {
                case.fields.iter().for_each(|f| result.push(*f));
                self.record_type_get_all_fields_of_variant(&case.variant, result);
            });
        }
    }
}

impl<'a> MutatingVisitorMut for SemanticCheckerVisitor<'a> {
    fn visit_program_heading(
        &mut self,
        node: &mut ast::ProgramHeading,
        span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        self.program_heading_loc = Some(*span);
        let x = &node.1;
        let mut already_seen = Vec::new();
        x.iter().for_each(|s| {
            // The presence of "input" / "output" makes them a defining point.
            // They are files.
            if already_seen.contains(s.get()) {
                self.diagnostics.add(
                    DiagnosticKind::Error,
                    *s.loc(),
                    format!("program parameter '{}' already declared", s.get()),
                );
            }
            already_seen.push(s.get().to_string());
            if s.get() == "input" || s.get() == "output" {
                let mut new_sym = Symbol::new();
                new_sym.set_name(s.get().as_str());
                new_sym.set_kind(SymbolKind::Variable);
                new_sym.set_type(self.ctx.type_system.get_textfile_type());
                new_sym.set_defining_point(*span);
                new_sym.set_required(true);
                new_sym.set_scope(self.ctx.scope.get_current_scope_id());

                let new_sym = self.ctx.new_symbol(new_sym);
                self.ctx.scope.add_entry(s.get().as_str(), new_sym);
            } else {
                self.ctx
                    .program_parameters
                    .push((s.get().to_string(), *s.loc()));
            }
        });
    }

    fn visit_pre_block(
        &mut self,
        _n: &mut ast::Block,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        // Note this also includes the program block.
        self.ctx.scope.push_scope(None);
        true
    }

    fn visit_post_block(&mut self, _n: &mut ast::Block, _span: &span::SpanLoc, _id: span::SpanId) {
        self.ctx.scope.pop_scope();
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
        new_sym.set_scope(self.ctx.scope.get_current_scope_id());

        if let Some(val) = self.ctx.get_ast_value(n.1.id()) {
            new_sym.set_const(val);
        } else {
            assert!(self.ctx.type_system.is_error_type(const_ty));
        }

        let new_sym = self.ctx.new_symbol(new_sym);
        self.ctx.scope.add_entry(name, new_sym);
        self.ctx.set_ast_symbol(n.0.id(), new_sym);
    }

    fn visit_pre_type_definition_part(
        &mut self,
        _n: &mut ast::TypeDefinitionPart,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        self.in_type_definition_part = true;
        true
    }

    fn visit_post_type_definition_part(
        &mut self,
        _n: &mut ast::TypeDefinitionPart,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        self.in_type_definition_part = false;
        for sym_id in &self.ctx.pending_type_definitions {
            let sym = self.ctx.get_symbol(*sym_id);
            let sym = sym.borrow();
            self.diagnostics.add(
                DiagnosticKind::Error,
                sym.get_defining_point().unwrap(),
                format!("type-identifier '{}' has not been defined", sym.get_name()),
            );
        }
        self.ctx.pending_type_definitions.clear();
    }

    fn visit_pre_type_definition(
        &mut self,
        n: &mut ast::TypeDefinition,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        // It may happen that this type definition is completing an earlier pending definition
        // So check that first.
        let name = n.0.get();
        let new_sym = if let Some(symbol_id) = self.is_pending_type_definition(name) {
            let sym = self.ctx.get_symbol(symbol_id);
            let mut sym = sym.borrow_mut();
            sym.set_kind(SymbolKind::Type);
            sym.set_defining_point(*n.0.loc());
            self.ctx.set_ast_symbol(n.0.id(), symbol_id);

            // Now remove this symbol from the pending set.
            self.ctx.remove_from_pending_definitions(symbol_id);

            symbol_id
        } else {
            // We do this in the pre visitor because we want to diagnose
            // redeclarations in the right order.
            if self.diagnose_redeclared_symbol(name, n.0.loc()) {
                return false;
            }

            let mut new_sym = Symbol::new();
            new_sym.set_name(name);
            new_sym.set_kind(SymbolKind::Type);
            new_sym.set_defining_point(*n.0.loc());
            new_sym.set_type(self.ctx.type_system.get_none_type());
            new_sym.set_scope(self.ctx.scope.get_current_scope_id());

            let new_sym = self.ctx.new_symbol(new_sym);
            self.ctx.scope.add_entry(name, new_sym);
            self.ctx.set_ast_symbol(n.0.id(), new_sym);

            new_sym
        };

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
            new_sym.set_scope(self.ctx.scope.get_current_scope_id());

            let new_sym = self.ctx.new_symbol(new_sym);
            self.ctx.scope.add_entry(constant_name, new_sym);
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
            new_sym.set_scope(self.ctx.scope.get_current_scope_id());

            let new_sym = self.ctx.new_symbol(new_sym);

            self.ctx.scope.add_entry(field_name.get(), new_sym);

            self.ctx.set_ast_symbol(field_name.id(), new_sym);

            // Remember this field.
            self.record_info
                .cases
                .last_mut()
                .unwrap()
                .fields
                .push(new_sym);
        }
    }

    fn visit_pre_variant(
        &mut self,
        n: &mut ast::Variant,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        // Constants
        let constants = &mut n.0;
        for constant in constants {
            let loc = *constant.loc();
            let id = constant.id();
            constant.get_mut().mutating_walk_mut(self, &loc, id);

            self.record_info
                .cases
                .last_mut()
                .unwrap()
                .constants
                .push(self.ctx.get_ast_value(id).unwrap());
        }

        // Field list
        let field_list = &mut n.1;
        let loc = *field_list.loc();
        let id = field_list.id();
        field_list.get_mut().mutating_walk_mut(self, &loc, id);

        // We're done
        false
    }

    fn visit_pre_variant_part(
        &mut self,
        n: &mut ast::VariantPart,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        // Keep the current variant.
        // This effectively clears the current record info.
        let mut previous_variant = std::mem::take(&mut self.record_info);

        // Walk first the variant selector.
        let variant_selector = &mut n.0;
        let loc = *variant_selector.loc();
        let id = variant_selector.id();
        variant_selector.get_mut().mutating_walk_mut(self, &loc, id);

        let variants = &mut n.1;
        for variant in variants {
            // This is each variant case
            self.record_info
                .cases
                .push(typesystem::VariantCase::default());

            let loc = *variant.loc();
            let id = variant.id();
            variant.get_mut().mutating_walk_mut(self, &loc, id);
        }

        // Now embed this internal variant to the enclosing one.
        previous_variant.cases.last_mut().unwrap().variant =
            Some(std::mem::take(&mut self.record_info));
        // Reinstate the previous variant info.
        self.record_info = previous_variant;

        // Now typecheck
        let variant_selector = &n.0;
        let variants = &n.1;
        let variant_selector_type = self.ctx.get_ast_type(variant_selector.id()).unwrap();
        if !self.ctx.type_system.is_error_type(variant_selector_type) {
            // Now check that each constant can be converted.
            for variant in variants {
                let consts = &variant.get().0;
                for const_ in consts {
                    let const_ty = self.ctx.get_ast_type(const_.id()).unwrap();
                    if !self.is_compatible(variant_selector_type, const_ty) {
                        self.diagnostics.add_with_extra(
                            DiagnosticKind::Error,
                            *const_.loc(),
                            format!(
                                "type {} of constant is not compatible with selector type {}",
                                self.ctx.type_system.get_type_name(const_ty),
                                self.ctx.type_system.get_type_name(variant_selector_type)
                            ),
                            vec![],
                            vec![Diagnostic::new(
                                DiagnosticKind::Info,
                                *variant_selector.loc(),
                                format!("corresponding variant selector declaration"),
                            )],
                        );
                    }
                }
            }
        }

        // We're done
        false
    }

    fn visit_post_variant_selector(
        &mut self,
        n: &mut ast::VariantSelector,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let type_denoter = self.ctx.get_ast_type(n.1.id()).unwrap();
        self.record_info.tag_type = type_denoter;

        if let Some(tag_name) = &n.0 {
            if !self.diagnose_redeclared_symbol(tag_name.get(), tag_name.loc()) {
                let mut new_sym = Symbol::new();
                new_sym.set_name(tag_name.get());
                new_sym.set_kind(SymbolKind::Field);
                new_sym.set_defining_point(*tag_name.loc());
                new_sym.set_type(type_denoter);
                new_sym.set_scope(self.ctx.scope.get_current_scope_id());

                let new_sym = self.ctx.new_symbol(new_sym);

                self.ctx.scope.add_entry(tag_name.get(), new_sym);

                self.ctx.set_ast_symbol(tag_name.id(), new_sym);

                // Remember this selector as a field.
                self.record_info.tag_name = new_sym;
            }
        } else {
            // We always create a fake symbol.
            let mut new_sym = Symbol::new();
            new_sym.set_name("{tag}");
            new_sym.set_kind(SymbolKind::Field);
            new_sym.set_defining_point(*span);
            new_sym.set_type(type_denoter);
            new_sym.set_scope(self.ctx.scope.get_current_scope_id());

            let new_sym = self.ctx.new_symbol(new_sym);

            // Remember this selector as a field.
            self.record_info.tag_name = new_sym;
        }

        self.ctx.set_ast_type(id, type_denoter);
    }

    fn visit_pre_field_list(
        &mut self,
        n: &mut ast::FieldList,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        // Fixed part of this field list.
        let fixed_part = &mut n.0;
        if let Some(fixed_part) = fixed_part {
            for record_section in fixed_part {
                let loc = *record_section.loc();
                let id = record_section.id();
                record_section.get_mut().mutating_walk_mut(self, &loc, id);
            }
        }

        // Variant part of this field list.
        let variant_part = &mut n.1;
        if let Some(variant_part) = variant_part {
            let loc = *variant_part.loc();
            let id = variant_part.id();
            variant_part.get_mut().mutating_walk_mut(self, &loc, id);
        }

        // Done
        false
    }

    fn visit_pre_record_type(
        &mut self,
        n: &mut ast::RecordType,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        let keep_record_info = std::mem::take(&mut self.record_info);

        // Create a new scope for the fields
        self.ctx.scope.push_scope(None);

        // Create a fake case, used only for the fixed part.
        self.record_info
            .cases
            .push(typesystem::VariantCase::default());

        // Walk the fields
        let field_list = &mut n.1;
        let field_list_loc = *field_list.loc();
        let field_list_id = field_list.id();
        field_list
            .get_mut()
            .mutating_walk_mut(self, &field_list_loc, field_list_id);

        // Leave the scope of the fields.
        self.ctx.scope.pop_scope();

        // Now gather all the info.
        let fixed_fields = std::mem::take(&mut self.record_info.cases[0].fields);
        let variant = std::mem::take(&mut self.record_info.cases[0].variant);

        // Compute all the fields for convenience.
        let mut all_fields = fixed_fields.clone();
        self.record_type_get_all_fields_of_variant(&variant, &mut all_fields);

        let packed = n.0.is_some();
        let mut new_record_type = Type::default();
        new_record_type.set_kind(TypeKind::Record {
            packed,
            fixed_fields,
            variant,
            all_fields,
        });

        let new_record_type = self.ctx.type_system.new_type(new_record_type);
        self.ctx.set_ast_type(id, new_record_type);

        self.record_info = keep_record_info;

        // We are done.
        false
    }

    fn visit_post_set_type(
        &mut self,
        n: &mut ast::SetType,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let packed = n.0.is_some();
        let element = self.ctx.get_ast_type(n.1.id()).unwrap();

        if self.ctx.type_system.is_error_type(element) {
            self.ctx.set_ast_type(id, element);
            return;
        }

        if !self.ctx.type_system.is_ordinal_type(element) {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *n.1.loc(),
                format!(
                    "element type of set {} is not an ordinal type",
                    self.ctx.type_system.get_type_name(element)
                ),
            );
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
            return;
        }

        let set_type = self.ctx.type_system.get_set_type(Some(packed), element);
        self.ctx.set_ast_type(id, set_type);
    }

    fn visit_post_file_type(
        &mut self,
        n: &mut ast::FileType,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let packed = n.0.is_some();
        let component_type = self.ctx.get_ast_type(n.1.id()).unwrap();
        if self.ctx.type_system.is_error_type(component_type) {
            self.ctx.set_ast_type(id, component_type);
            return;
        }

        if !self
            .ctx
            .type_system
            .is_valid_component_type_of_file_type(component_type)
        {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *n.1.loc(),
                format!(
                    "type {} is not a valid component type for a file type",
                    self.ctx.type_system.get_type_name(component_type)
                ),
            );
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
            return;
        }

        let file_type = self.ctx.type_system.get_file_type(packed, component_type);
        self.ctx.set_ast_type(id, file_type);
    }

    fn visit_pre_pointer_type(
        &mut self,
        _n: &mut ast::PointerType,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        self.in_pointer_type = true;
        true
    }

    fn visit_post_pointer_type(
        &mut self,
        n: &mut ast::PointerType,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let mut body = || {
            let pointee_type = self.ctx.get_ast_type(n.0.id()).unwrap();

            if self.ctx.type_system.is_error_type(pointee_type) {
                self.ctx.set_ast_type(id, pointee_type);
                return;
            }

            let pointer_type = self.ctx.type_system.get_pointer_type(pointee_type);
            self.ctx.set_ast_type(id, pointer_type);
        };

        body();
        self.in_pointer_type = false;
    }

    fn visit_pre_procedure_forward(
        &mut self,
        n: &mut ast::ProcedureForward,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        let proc_name = n.0.get();

        let redeclaration_status =
            self.diagnose_reintroduced_procedure_declaration(proc_name, n.0.loc());
        match redeclaration_status {
            FunctionProcedureDeclarationStatus::AlreadyDefined(_sym)
            | FunctionProcedureDeclarationStatus::AlreadyDeclared(_sym) => {
                // This is an error.
                return false;
            }
            FunctionProcedureDeclarationStatus::ForwardDeclared(_) => {
                // This is fine but requires checking equivalence.
            }
            FunctionProcedureDeclarationStatus::NotDeclared => {
                // Fine, this is a new declaration.
            }
        }

        if is_required_procedure_or_function(&proc_name) {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *n.0.loc(),
                format!(
                    "cannot declare required {} '{}'",
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

        let proc_decl_scope_id = self.ctx.scope.get_current_scope_id();

        self.ctx.scope.push_scope(None);

        let formal_parameters = self.gather_formal_parameters(&mut n.1);
        // No parameters here means zero parameters in a forward declaration.
        let formal_parameters = formal_parameters.unwrap_or_else(|| vec![]);

        match redeclaration_status {
            FunctionProcedureDeclarationStatus::ForwardDeclared(prev_sym_id)
            | FunctionProcedureDeclarationStatus::AlreadyDefined(prev_sym_id) => {
                if !self.equivalent_procedure_declarations(prev_sym_id, &formal_parameters) {
                    let prev_sym = self.ctx.get_symbol(prev_sym_id);
                    let prev_sym = prev_sym.borrow();
                    self.diagnostics.add_with_extra(DiagnosticKind::Error, *n.0.loc(),
                    format!("procedure declaration is incompatible with a previous procedure declaration"),
                     vec![],
                     vec![Diagnostic::new(DiagnosticKind::Info, prev_sym.get_defining_point().unwrap(), format!("previous declaration"))]);
                }
                self.ctx.scope.pop_scope();
                // Nothing else to do at this point.
                return false;
            }
            FunctionProcedureDeclarationStatus::NotDeclared => {
                // Register symbol as usual. Happens later.
            }
            _ => {
                panic!("Unexpected case");
            }
        }

        let proc_sym_id = self.create_new_procedure_symbol(
            proc_decl_scope_id,
            proc_name,
            formal_parameters,
            /* is_definition */ false,
            *n.0.loc(),
        );
        self.ctx.scope.set_scope_symbol(Some(proc_sym_id));
        self.ctx.set_ast_symbol(n.0.id(), proc_sym_id);

        self.ctx.scope.pop_scope();
        false
    }

    fn visit_pre_function_forward(
        &mut self,
        n: &mut ast::FunctionForward,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        let function_name = n.0.get();

        let redeclaration_status =
            self.diagnose_reintroduced_function_declaration(function_name, n.0.loc());
        match redeclaration_status {
            FunctionProcedureDeclarationStatus::AlreadyDefined(_)
            | FunctionProcedureDeclarationStatus::AlreadyDeclared(_) => {
                // This is an error.
                return false;
            }
            FunctionProcedureDeclarationStatus::ForwardDeclared(_) => {
                // This is fine but requires checking equivalence.
            }
            FunctionProcedureDeclarationStatus::NotDeclared => {
                // Fine, this is a new declaration.
            }
        }

        if is_required_procedure_or_function(&function_name) {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *n.0.loc(),
                format!(
                    "cannot declare required {} '{}'",
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

        let function_decl_scope_id = self.ctx.scope.get_current_scope_id();

        self.ctx.scope.push_scope(None);

        let formal_parameters = self.gather_formal_parameters(&mut n.1);
        // In a forward declaration, no parameters means 0 parameters.
        let formal_parameters = formal_parameters.unwrap_or_else(|| vec![]);
        let result_type = self.gather_return_type(&mut n.2);

        match redeclaration_status {
            FunctionProcedureDeclarationStatus::ForwardDeclared(prev_sym_id)
            | FunctionProcedureDeclarationStatus::AlreadyDefined(prev_sym_id) => {
                if !self.equivalent_function_declarations(
                    prev_sym_id,
                    &formal_parameters,
                    result_type,
                ) {
                    let prev_sym = self.ctx.get_symbol(prev_sym_id);
                    let prev_sym = prev_sym.borrow();
                    self.diagnostics.add_with_extra(DiagnosticKind::Error, *n.0.loc(),
                    format!("function declaration is incompatible with a previous function declaration"),
                     vec![],
                     vec![Diagnostic::new(DiagnosticKind::Info, prev_sym.get_defining_point().unwrap(), format!("previous declaration"))]);
                }
                self.ctx.scope.pop_scope();
                // Nothing else to do at this point.
                return false;
            }
            FunctionProcedureDeclarationStatus::NotDeclared => {
                // Register symbol as usual. Happens later.
            }
            _ => {
                panic!("Unexpected case");
            }
        }

        let function_sym_id = self.create_new_function_symbol(
            function_decl_scope_id,
            function_name,
            formal_parameters,
            result_type,
            /* is_definition */ false,
            *n.0.loc(),
        );
        self.ctx.scope.set_scope_symbol(Some(function_sym_id));
        self.ctx.set_ast_symbol(n.0.id(), function_sym_id);

        self.ctx.scope.pop_scope();
        false
    }

    fn visit_pre_function_late_definition(
        &mut self,
        n: &mut ast::FunctionLateDefinition,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        let function_name = n.0.get();

        let redeclaration_status =
            self.diagnose_reintroduced_function_definition(function_name, n.0.loc());
        match redeclaration_status {
            FunctionProcedureDeclarationStatus::AlreadyDeclared(_)
            | FunctionProcedureDeclarationStatus::AlreadyDefined(_) => {
                // This is an error.
                return false;
            }
            FunctionProcedureDeclarationStatus::ForwardDeclared(_) => {
                // Fine. Just assume the parameters and the return type.
            }
            FunctionProcedureDeclarationStatus::NotDeclared => {
                // This requires a previous declaration.
                self.diagnostics.add(
                    DiagnosticKind::Error,
                    *n.0.loc(),
                    format!("this form of function definition requires an earlier forward function declaration"),
                );
                return false;
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

        self.ctx.scope.push_scope(None);

        let function_sym_id = match redeclaration_status {
            FunctionProcedureDeclarationStatus::ForwardDeclared(prev_sym_id) => {
                let prev_formal_parameters = {
                    let prev_sym = self.ctx.get_symbol(prev_sym_id);
                    let prev_sym = prev_sym.borrow();
                    prev_sym.get_formal_parameters().unwrap()
                };

                // We also need to insert the formal parameters in the scope, so name resolution works.
                for prev_formal_param_sym_id in prev_formal_parameters.iter().flatten() {
                    let prev_formal_param = self.ctx.get_symbol(*prev_formal_param_sym_id);
                    let mut prev_formal_param = prev_formal_param.borrow_mut();
                    prev_formal_param.set_scope(self.ctx.scope.get_current_scope_id());

                    self.ctx
                        .scope
                        .add_entry(&prev_formal_param.get_name(), *prev_formal_param_sym_id);
                }

                let prev_sym = self.ctx.get_symbol(prev_sym_id);
                let mut prev_sym = prev_sym.borrow_mut();
                prev_sym.set_defined(true);
                prev_sym.set_defining_point(*n.0.loc());

                let return_sym_id = prev_sym.get_return_symbol().unwrap();
                let return_sym = self.ctx.get_symbol(return_sym_id);
                let mut return_sym = return_sym.borrow_mut();
                return_sym.set_defining_point(*n.0.loc());

                prev_sym_id
            }
            _ => {
                unreachable!();
            }
        };

        self.ctx.scope.set_scope_symbol(Some(function_sym_id));
        self.ctx.set_ast_symbol(n.0.id(), function_sym_id);

        let function_block = &mut n.1;
        let loc = *function_block.loc();
        let id = function_block.id();
        function_block.get_mut().mutating_walk_mut(self, &loc, id);

        self.ctx.scope.pop_scope();

        false
    }

    fn visit_type_identifier(
        &mut self,
        node: &mut ast::TypeIdentifier,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let context_allows_undeclared_types = self.in_type_definition_part && self.in_pointer_type;
        let context_disallows_undeclared_types = !context_allows_undeclared_types;

        if let Some(symbol_id) = self.lookup_symbol_and_diagnose(
            node.0.get(),
            node.0.loc(),
            context_disallows_undeclared_types,
        ) {
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
                SymbolKind::PendingTypeDefinition => {
                    let mut new_type = Type::default();
                    new_type.set_kind(TypeKind::NamedType(type_name.id()));

                    let ty = self.ctx.type_system.new_type(new_type);
                    self.ctx.set_ast_type(id, ty);
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
            if context_allows_undeclared_types {
                // This is a special case in which we see ourselves forced to have a pending type definition
                // that we will check upon leaving the type definition part.
                if let Some(symbol_id) = self.lookup_symbol(node.0.get(), node.0.loc()) {
                    let type_name = self.ctx.get_symbol(symbol_id);
                    let mut type_name = type_name.borrow_mut();
                    match type_name.get_kind() {
                        SymbolKind::ErrorLookup => {
                            type_name.set_kind(SymbolKind::PendingTypeDefinition);
                            // We need a type that is not error.
                            type_name.set_type(self.ctx.type_system.get_none_type());
                            self.ctx.add_to_pending_definitions(symbol_id);

                            let mut new_type = Type::default();
                            new_type.set_kind(TypeKind::NamedType(type_name.id()));

                            let ty = self.ctx.type_system.new_type(new_type);
                            self.ctx.set_ast_type(id, ty);
                        }
                        _ => {
                            panic!("The generated symbol had to be an error lookup!");
                        }
                    }
                } else {
                    panic!("This should have generated a symbol!")
                }
            } else {
                self.ctx
                    .set_ast_type(id, self.ctx.type_system.get_error_type());
            }
        }
    }

    fn visit_post_variable_declaration_part(
        &mut self,
        _n: &mut ast::VariableDeclarationPart,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        if self.ctx.scope.get_current_scope_id() == self.ctx.scope.get_global_scope_id() {
            // Check whether program parameters have been declared.
            let program_parameters = self.ctx.program_parameters.clone();
            for (program_param, loc) in program_parameters {
                if let Some(sym_id) = self.lookup_symbol(&program_param, &loc) {
                    let sym = self.ctx.get_symbol(sym_id);
                    let ty = sym.borrow().get_type().unwrap();
                    if !self.ctx.type_system.is_file_type(ty) {
                        self.diagnostics.add_with_extra(
                            DiagnosticKind::Error,
                            sym.borrow().get_defining_point().unwrap(),
                            format!("only file types are allowed as program parameters"),
                            vec![],
                            vec![Diagnostic::new(
                                DiagnosticKind::Info,
                                loc,
                                format!("declaration of program parameter"),
                            )],
                        );
                    } else {
                        self.ctx.global_files.push(sym_id);
                    }
                }
            }
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
            new_sym.set_scope(self.ctx.scope.get_current_scope_id());

            let new_sym = self.ctx.new_symbol(new_sym);

            self.ctx.scope.add_entry(e.get(), new_sym);

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
            let (sym_kind, sym_type, scope_id) = {
                let var_name = self.ctx.get_symbol(sym_id);
                let var_name = var_name.borrow();
                (
                    var_name.get_kind(),
                    var_name.get_type(),
                    var_name.get_scope().unwrap(),
                )
            };

            match sym_kind {
                SymbolKind::Variable => {
                    self.ctx.set_ast_symbol(id, sym_id);
                    self.ctx.set_ast_type(id, sym_type.unwrap());

                    // Now let's analyse in which scopes these variables live.
                    let symbol_of_current_scope = self
                        .ctx
                        .scope
                        .get_innermost_scope_symbol(self.ctx.scope.get_current_scope_id());
                    let symbol_of_variable_scope =
                        self.ctx.scope.get_innermost_scope_symbol(scope_id);

                    match (symbol_of_current_scope, symbol_of_variable_scope) {
                        (Some(symbol_of_current_scope), Some(symbol_of_variable_scope)) => {
                            if symbol_of_current_scope != symbol_of_variable_scope {
                                let symbol_of_current_scope =
                                    self.ctx.get_symbol(symbol_of_current_scope);
                                let mut symbol_of_current_scope =
                                    symbol_of_current_scope.borrow_mut();

                                assert!(
                                    symbol_of_current_scope.get_kind() == SymbolKind::Function
                                        || symbol_of_current_scope.get_kind()
                                            == SymbolKind::Procedure
                                );
                                symbol_of_current_scope.add_to_required_environment(sym_id);
                            }
                        }
                        (Some(_), None) => {
                            // We're in a non-global scope referring to a global-scope variable.
                        }
                        (None, None) => {
                            // We're in a global scope referring to a global-scope variable. This typically happens in the main program statement.
                        }
                        _ => unreachable!(),
                    }
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
            if !self.ctx.type_system.is_array_type(current_ty)
                && !self.ctx.type_system.is_conformable_array_type(current_ty)
            {
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
                .array_or_conformable_array_type_get_component_type(current_ty);
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
        let record_fields = self.ctx.type_system.record_type_get_all_fields(base_type);
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

    fn visit_post_assig_pointer_deref(
        &mut self,
        n: &mut ast::AssigPointerDeref,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let deref_type = self.ctx.get_ast_type(n.0.id()).unwrap();
        if self.ctx.type_system.is_error_type(deref_type) {
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
            return;
        }

        if !self.ctx.type_system.is_pointer_type(deref_type)
            && !self.ctx.type_system.is_file_type(deref_type)
        {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *n.0.loc(),
                format!(
                    "expression has type {} which is not a pointer type or a file type",
                    self.ctx.type_system.get_type_name(deref_type)
                ),
            );
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
            return;
        }

        let value_type = if self.ctx.type_system.is_pointer_type(deref_type) {
            self.ctx
                .type_system
                .pointer_type_get_pointee_type(deref_type)
        } else if self.ctx.type_system.is_file_type(deref_type) {
            self.ctx
                .type_system
                .file_type_get_component_type(deref_type)
        } else {
            unreachable!("Unexpected type");
        };
        self.ctx.set_ast_type(id, value_type);
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
                        if let Some(sym_id) = self.ctx.scope.lookup(x.0.get()) {
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
                                SymbolKind::BoundIdentifier => {
                                    // Bound identifiers are not variables.
                                    let new_bound_variable = ast::ExprBoundIdentifier(
                                        span::Spanned::new(*span, x.0.get().clone()),
                                    );

                                    self.ctx.set_ast_symbol(id, sym_id);
                                    self.ctx.set_ast_type(id, sym_type.unwrap());

                                    let new_bound_variable =
                                        ast::Expr::BoundIdentifier(new_bound_variable);
                                    *n = new_bound_variable;
                                    return false;
                                }
                                _ => {}
                            }
                        } else if is_required_function_zeroadic(x.0.get()) {
                            // Some required functions can be invoked as zero parameters.
                            let new_call_expr = ast::ExprFunctionCall(
                                span::Spanned::new(*x.0.loc(), x.0.get().clone()),
                                vec![],
                            );

                            let return_type =
                                self.ctx.required_function_zeroadic_return_type(x.0.get());

                            let new_call = ast::Expr::FunctionCall(new_call_expr);
                            self.ctx.set_ast_type(id, return_type);

                            *n = new_call;
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        true
    }

    fn visit_post_expr_range(
        &mut self,
        n: &mut ast::ExprRange,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let lower_bound = &n.0;
        let lower_bound_ty = self.ctx.get_ast_type(lower_bound.id()).unwrap();

        let upper_bound = &n.1;
        let upper_bound_ty = self.ctx.get_ast_type(upper_bound.id()).unwrap();

        if self.ctx.type_system.is_error_type(lower_bound_ty)
            || self.ctx.type_system.is_error_type(upper_bound_ty)
        {
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
            return;
        }

        for (e, ty) in [(lower_bound, lower_bound_ty), (upper_bound, upper_bound_ty)] {
            if !self.ctx.type_system.is_ordinal_type(ty) {
                self.diagnostics.add(
                    DiagnosticKind::Error,
                    *e.loc(),
                    format!("expression must be of ordinal type"),
                );
                self.ctx
                    .set_ast_type(id, self.ctx.type_system.get_error_type());
                return;
            }
        }

        if !self
            .ctx
            .type_system
            .same_type(lower_bound_ty, upper_bound_ty)
        {
            self.diagnostics.add_with_extra(
                DiagnosticKind::Error,
                *upper_bound.loc(),
                format!(
                    "upper bound expression is of type {} but it should be of type {}",
                    self.ctx.type_system.get_type_name(upper_bound_ty),
                    self.ctx.type_system.get_type_name(lower_bound_ty)
                ),
                vec![],
                vec![Diagnostic::new(
                    DiagnosticKind::Info,
                    *lower_bound.loc(),
                    format!(
                        "lower bound expression is of type {}",
                        self.ctx.type_system.get_type_name(lower_bound_ty)
                    ),
                )],
            );
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
            return;
        }

        self.ctx.set_ast_type(id, lower_bound_ty);
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
        if let Some(scope_symbol_id) = self
            .ctx
            .scope
            .get_innermost_scope_symbol(self.ctx.scope.get_current_scope_id())
        {
            let scope_symbol = self.ctx.get_symbol(scope_symbol_id);
            let scope_symbol = scope_symbol.borrow();
            if scope_symbol.get_kind() == SymbolKind::Function {
                match lhs.get_mut() {
                    ast::Assig::Variable(assig_var) => {
                        let name = assig_var.0.get();
                        if let Some(symbol_id) = self.ctx.scope.lookup(&name) {
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
                } else if self.is_pointer_and_generic_pointer(lhs_type, rhs_type)
                    || self.is_set_and_generic_set(lhs_type, rhs_type)
                {
                    // Convert it into a pointer type the lhs.
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
        _span: &span::SpanLoc,
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
                let op_type = self.valid_for_relational_nonstrict(lhs_ty, rhs_ty);
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
                        } else if self.is_set_and_generic_set(lhs_ty, rhs_ty) {
                            self.ctx.set_ast_type(node.2.id(), lhs_ty);
                        } else if self.is_set_and_generic_set(rhs_ty, lhs_ty) {
                            self.ctx.set_ast_type(node.1.id(), rhs_ty);
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
                let op_type = self.valid_for_relational_strict(lhs_ty, rhs_ty);
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
                        } else if self.is_set_and_generic_set(lhs_ty, rhs_ty) {
                            self.ctx.set_ast_type(node.2.id(), lhs_ty);
                        } else if self.is_set_and_generic_set(rhs_ty, lhs_ty) {
                            self.ctx.set_ast_type(node.1.id(), rhs_ty);
                        } else if self.is_pointer_and_generic_pointer(lhs_ty, rhs_ty) {
                            self.ctx.set_ast_type(node.2.id(), lhs_ty);
                        } else if self.is_pointer_and_generic_pointer(rhs_ty, lhs_ty) {
                            self.ctx.set_ast_type(node.1.id(), rhs_ty);
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
                if self.ctx.type_system.is_set_type(rhs_ty)
                    && self
                        .ctx
                        .type_system
                        .same_type(self.ctx.type_system.set_type_get_element(rhs_ty), lhs_ty)
                {
                    self.ctx
                        .set_ast_type(id, self.ctx.type_system.get_bool_type());
                } else {
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
                        if let Some(set_ty) = self.same_set_type(lhs_ty, rhs_ty) {
                            self.ctx.set_ast_type(id, set_ty);
                            if self.ctx.type_system.is_generic_set_type(lhs_ty) {
                                self.ctx.set_ast_type(node.1.id(), set_ty);
                            }
                            if self.ctx.type_system.is_generic_set_type(rhs_ty) {
                                self.ctx.set_ast_type(node.2.id(), set_ty);
                            }
                        } else {
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
            }
            BinOperand::LogicalOr | BinOperand::LogicalAnd => {
                let op_type = self.both_bool_type(lhs_ty, rhs_ty);
                match op_type {
                    Some(result_type) => {
                        self.ctx.set_ast_type(id, result_type);
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

        if is_required_procedure(callee.get()) {
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

        if is_required_function(callee.get()) {
            args.iter_mut().for_each(|arg| {
                let loc = *arg.loc();
                let id = arg.id();
                arg.get_mut().mutating_walk_mut(self, &loc, id);
            });

            let function_name = callee.get();
            match function_name.as_str() {
                "eof" => {
                    match args.len() {
                        0 => {
                            self.ensure_input(callee.loc());
                        }
                        1 => {
                            let ty = self.ctx.get_ast_type(args[0].id()).unwrap();
                            if !self.ctx.type_system.is_file_type(ty) {
                                self.diagnostics.add(
                                    DiagnosticKind::Error,
                                    *callee.loc(),
                                    format!("argument of eof must have file type"),
                                );
                                self.ctx
                                    .set_ast_type(id, self.ctx.type_system.get_error_type());
                                return false;
                            }
                        }
                        _ => {
                            self.diagnostics.add(
                                DiagnosticKind::Error,
                                *span,
                                format!("too many arguments in call to eof"),
                            );
                            self.ctx
                                .set_ast_type(id, self.ctx.type_system.get_error_type());
                            return false;
                        }
                    }
                    self.ctx
                        .set_ast_type(id, self.ctx.type_system.get_bool_type());
                }
                "eoln" => {
                    match args.len() {
                        0 => {
                            self.ensure_input(callee.loc());
                        }
                        1 => {
                            let ty = self.ctx.get_ast_type(args[0].id()).unwrap();
                            if !self.ctx.type_system.is_textfile_type(ty) {
                                self.diagnostics.add(
                                    DiagnosticKind::Error,
                                    *args[0].loc(),
                                    format!("argument of eoln must have textfile type"),
                                );
                                self.ctx
                                    .set_ast_type(id, self.ctx.type_system.get_error_type());
                                return false;
                            }
                        }
                        _ => {
                            self.diagnostics.add(
                                DiagnosticKind::Error,
                                *span,
                                format!("too many arguments in call to eoln"),
                            );
                            self.ctx
                                .set_ast_type(id, self.ctx.type_system.get_error_type());
                            return false;
                        }
                    }
                    self.ctx
                        .set_ast_type(id, self.ctx.type_system.get_bool_type());
                }
                "abs" | "sqr" | "sin" | "cos" | "exp" | "ln" | "sqrt" | "arctan" => {
                    match args.len() {
                        1 => {
                            let ty = self.ctx.get_ast_type(args[0].id()).unwrap();
                            if !self.ctx.type_system.is_integer_type(ty)
                                && !self.ctx.type_system.is_real_type(ty)
                            {
                                self.diagnostics.add(
                                    DiagnosticKind::Error,
                                    *args[0].loc(),
                                    format!(
                                        "argument of {} must have integer or real type",
                                        function_name
                                    ),
                                );
                                self.ctx
                                    .set_ast_type(id, self.ctx.type_system.get_error_type());
                                return false;
                            }
                            if function_name == "abs" || function_name == "sqr" {
                                self.ctx.set_ast_type(id, ty);
                            } else {
                                if self.ctx.type_system.is_integer_type(ty) {
                                    // Make a conversion here.
                                    let conversion = SemanticCheckerVisitor::create_conversion_expr(
                                        args[0].take(),
                                    );
                                    args[0].reset(conversion);
                                    self.ctx.set_ast_type(
                                        args[0].id(),
                                        self.ctx.type_system.get_real_type(),
                                    );
                                }
                                self.ctx
                                    .set_ast_type(id, self.ctx.type_system.get_real_type());
                            }
                        }
                        _ => {
                            self.diagnostics.add(
                                DiagnosticKind::Error,
                                *span,
                                format!("function {} requires one argument", function_name),
                            );
                            self.ctx
                                .set_ast_type(id, self.ctx.type_system.get_error_type());
                            return false;
                        }
                    }
                }
                "trunc" | "round" => match args.len() {
                    1 => {
                        let ty = self.ctx.get_ast_type(args[0].id()).unwrap();
                        if !self.ctx.type_system.is_real_type(ty) {
                            self.diagnostics.add(
                                DiagnosticKind::Error,
                                *args[0].loc(),
                                format!("argument of {} must have real type", function_name),
                            );
                            self.ctx
                                .set_ast_type(id, self.ctx.type_system.get_error_type());
                            return false;
                        }
                        self.ctx
                            .set_ast_type(id, self.ctx.type_system.get_integer_type());
                    }
                    _ => {
                        self.diagnostics.add(
                            DiagnosticKind::Error,
                            *span,
                            format!("function {} requires one argument", function_name),
                        );
                        self.ctx
                            .set_ast_type(id, self.ctx.type_system.get_error_type());
                        return false;
                    }
                },
                "ord" => match args.len() {
                    1 => {
                        let ty = self.ctx.get_ast_type(args[0].id()).unwrap();
                        if !self.ctx.type_system.is_ordinal_type(ty) {
                            self.diagnostics.add(
                                DiagnosticKind::Error,
                                *args[0].loc(),
                                format!("argument of {} must have ordinal type", function_name),
                            );
                            self.ctx
                                .set_ast_type(id, self.ctx.type_system.get_error_type());
                            return false;
                        }
                        self.ctx
                            .set_ast_type(id, self.ctx.type_system.get_integer_type());
                    }
                    _ => {
                        self.diagnostics.add(
                            DiagnosticKind::Error,
                            *span,
                            format!("function {} requires one argument", function_name),
                        );
                        self.ctx
                            .set_ast_type(id, self.ctx.type_system.get_error_type());
                        return false;
                    }
                },
                "chr" => match args.len() {
                    1 => {
                        let ty = self.ctx.get_ast_type(args[0].id()).unwrap();
                        if !self.ctx.type_system.is_integer_type(ty) {
                            self.diagnostics.add(
                                DiagnosticKind::Error,
                                *args[0].loc(),
                                format!("argument of {} must have integer type", function_name),
                            );
                            self.ctx
                                .set_ast_type(id, self.ctx.type_system.get_error_type());
                            return false;
                        }
                        self.ctx
                            .set_ast_type(id, self.ctx.type_system.get_char_type());
                    }
                    _ => {
                        self.diagnostics.add(
                            DiagnosticKind::Error,
                            *span,
                            format!("function {} requires one argument", function_name),
                        );
                        self.ctx
                            .set_ast_type(id, self.ctx.type_system.get_error_type());
                        return false;
                    }
                },
                "succ" | "pred" => match args.len() {
                    1 => {
                        let ty = self.ctx.get_ast_type(args[0].id()).unwrap();
                        if !self.ctx.type_system.is_ordinal_type(ty) {
                            self.diagnostics.add(
                                DiagnosticKind::Error,
                                *args[0].loc(),
                                format!("argument of {} must have ordinal type", function_name),
                            );
                            self.ctx
                                .set_ast_type(id, self.ctx.type_system.get_error_type());
                            return false;
                        }
                        self.ctx.set_ast_type(id, ty);
                    }
                    _ => {
                        self.diagnostics.add(
                            DiagnosticKind::Error,
                            *span,
                            format!("function {} requires one argument", function_name),
                        );
                        self.ctx
                            .set_ast_type(id, self.ctx.type_system.get_error_type());
                        return false;
                    }
                },
                "odd" => match args.len() {
                    1 => {
                        let ty = self.ctx.get_ast_type(args[0].id()).unwrap();
                        if !self.ctx.type_system.is_integer_type(ty) {
                            self.diagnostics.add(
                                DiagnosticKind::Error,
                                *args[0].loc(),
                                format!("argument of {} must have integer type", function_name),
                            );
                            self.ctx
                                .set_ast_type(id, self.ctx.type_system.get_error_type());
                            return false;
                        }
                        self.ctx
                            .set_ast_type(id, self.ctx.type_system.get_bool_type());
                    }
                    _ => {
                        self.diagnostics.add(
                            DiagnosticKind::Error,
                            *span,
                            format!("function {} requires one argument", function_name),
                        );
                        self.ctx
                            .set_ast_type(id, self.ctx.type_system.get_bool_type());
                        return false;
                    }
                },
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
        } else if let Some(callee_symbol_id) = self.lookup_symbol(callee.get(), callee.loc()) {
            let callee_symbol_kind = self.ctx.get_symbol(callee_symbol_id).borrow().get_kind();
            match callee_symbol_kind {
                SymbolKind::Function => {
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

    fn visit_post_expr_set_literal(
        &mut self,
        n: &mut ast::ExprSetLiteral,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        if n.0.is_empty() {
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_generic_set_type());
            return;
        }

        let element_types: Vec<_> =
            n.0.iter()
                .map(|x| (self.ctx.get_ast_type(x.id()).unwrap(), x.loc()))
                .collect();
        let (first_element_type, first_element_loc) = element_types[0];

        if self.ctx.type_system.is_error_type(first_element_type) {
            self.ctx.set_ast_type(id, first_element_type);
            return;
        }

        if !self.ctx.type_system.is_ordinal_type(first_element_type) {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *first_element_loc,
                format!(
                    "member of set has type {} that is not an ordinal type",
                    self.ctx.type_system.get_type_name(first_element_type)
                ),
            );
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
            return;
        }

        let mut all_ok = true;
        for i in 1..element_types.len() {
            let (current_element_type, current_element_loc) = element_types[i];
            if self.ctx.type_system.is_error_type(current_element_type) {
                all_ok = false;
            } else if !self
                .ctx
                .type_system
                .same_type(first_element_type, current_element_type)
            {
                all_ok = false;
                self.diagnostics.add_with_extra(
                    DiagnosticKind::Error,
                    *current_element_loc,
                    format!(
                        "type of set member is {} which is not the same as {}",
                        self.ctx.type_system.get_type_name(current_element_type),
                        self.ctx.type_system.get_type_name(first_element_type)
                    ),
                    vec![*first_element_loc],
                    vec![],
                );
            }
        }

        if !all_ok {
            self.ctx
                .set_ast_type(id, self.ctx.type_system.get_error_type());
            return;
        }

        let set_type = self.ctx.type_system.get_set_type(None, first_element_type);

        self.ctx.set_ast_type(id, set_type);
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

    fn visit_const_nil(&mut self, _n: &mut ast::ConstNil, _span: &span::SpanLoc, id: span::SpanId) {
        self.ctx
            .set_ast_type(id, self.ctx.type_system.get_generic_pointer_type());
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
        let args = &mut node.1;
        let procedure_name = callee.get().as_str();
        if is_required_procedure(procedure_name) {
            // Required procedures may have all the arguments typecheked in a row.
            if let Some(args) = args {
                args.iter_mut().for_each(|arg| {
                    let loc = *arg.loc();
                    let id = arg.id();
                    arg.get_mut().mutating_walk_mut(self, &loc, id);
                });
            }
            match procedure_name {
                "read" | "readln" => {
                    let is_readln = procedure_name == "readln";
                    if let Some(args) = &mut node.1 {
                        let (first_arg, file_component, is_textfile) =
                            self.analyze_write_read_args("read", "input", is_readln, span, args);
                        for arg in &mut args[first_arg..] {
                            match arg.get() {
                                ast::Expr::Variable(_) => {
                                    let ty = self.ctx.get_ast_type(arg.id()).unwrap();
                                    if is_textfile {
                                        if !self.ctx.type_system.is_error_type(ty)
                                            && !self.ctx.type_system.is_real_type(ty)
                                            // This looks like a mistake in the
                                            // Basic spec, in which it appears
                                            // mentioned as a valid variable but
                                            // never describes its semantics.
                                            /* && !self.ctx.type_system.is_string_type(ty) */
                                            && !self.is_compatible(
                                                self.ctx.type_system.get_char_type(),
                                                ty,
                                            )
                                            && !self.is_compatible(
                                                self.ctx.type_system.get_integer_type(),
                                                ty,
                                            )
                                        {
                                            self.diagnostics.add(
                                                DiagnosticKind::Error,
                                                *arg.loc(),
                                                format!(
                                                    "variable of type {} is not valid for textfile",
                                                    self.ctx.type_system.get_type_name(ty)
                                                ),
                                            );
                                        }
                                    } else {
                                        if !self.ctx.type_system.is_error_type(file_component)
                                            && !self.ctx.type_system.is_error_type(ty)
                                            && self.is_assignment_compatible(ty, file_component)
                                        {
                                            if !self.ctx.type_system.same_type(ty, file_component) {
                                                let conversion =
                                                    SemanticCheckerVisitor::create_conversion_expr(
                                                        arg.take(),
                                                    );
                                                arg.reset(conversion);
                                                self.ctx.set_ast_type(arg.id(), ty);
                                            }
                                        } else if !self
                                            .ctx
                                            .type_system
                                            .is_error_type(file_component)
                                            && !self.ctx.type_system.is_error_type(ty)
                                        {
                                            self.diagnostics.add(
                                                DiagnosticKind::Error,
                                                *arg.loc(),
                                                format!("variable of type {} is not assignment compatible with the file component type {}",
                                                self.ctx.type_system.get_type_name(ty),
                                                self.ctx.type_system.get_type_name(file_component))
                                            );
                                        }
                                    }
                                }
                                _ => {
                                    self.diagnostics.add(
                                        DiagnosticKind::Error,
                                        *arg.loc(),
                                        format!("must be a variable"),
                                    );
                                }
                            }
                        }
                    } else {
                        if !is_readln {
                            self.diagnostics.add(
                                DiagnosticKind::Error,
                                *span,
                                format!("too few arguments in call to read"),
                            );
                        } else {
                            self.ensure_input(span);
                        }
                    }
                }
                "write" | "writeln" => {
                    let is_writeln = procedure_name == "writeln";
                    if let Some(args) = &node.1 {
                        let (first_arg, file_component, is_textfile) =
                            self.analyze_write_read_args("write", "output", is_writeln, span, args);
                        for arg in &args[first_arg..] {
                            let ty = self.ctx.get_ast_type(arg.id()).unwrap();
                            if is_textfile {
                                if !self.ctx.type_system.is_error_type(ty)
                                    && !self.ctx.type_system.is_integer_type(ty)
                                    && !self.ctx.type_system.is_real_type(ty)
                                    && !self.ctx.type_system.is_char_type(ty)
                                    && !self.ctx.type_system.is_bool_type(ty)
                                    && !self.ctx.type_system.is_string_type(ty)
                                {
                                    self.diagnostics.add(
                                        DiagnosticKind::Error,
                                        *arg.loc(),
                                        format!(
                                            "argument of type {} is not valid for a textfile",
                                            self.ctx.type_system.get_type_name(ty)
                                        ),
                                    );
                                }
                            } else {
                                match arg.get() {
                                    ast::Expr::WriteParameter(..) => {
                                        self.diagnostics.add(
                                                DiagnosticKind::Error,
                                                *arg.loc(),
                                                format!("a write to a non-textfile does not allow this kind of argument")
                                            );
                                    }
                                    _ => {}
                                }
                                if !self.ctx.type_system.is_error_type(file_component)
                                    && !self.ctx.type_system.is_error_type(ty)
                                    && !self.is_assignment_compatible(file_component, ty)
                                {
                                    self.diagnostics.add(
                                                DiagnosticKind::Error,
                                                *arg.loc(),
                                                format!("argument of write is of type {} not assignment compatible with the file component type {}",
                                                self.ctx.type_system.get_type_name(ty),
                                                self.ctx.type_system.get_type_name(file_component))
                                            );
                                }
                            }
                        }
                    } else {
                        if !is_writeln {
                            self.diagnostics.add(
                                DiagnosticKind::Error,
                                *span,
                                format!("too few arguments in call to write"),
                            );
                        } else {
                            self.ensure_output(span);
                        }
                    }
                }
                "new" | "dispose" => {
                    if let Some(args) = &node.1 {
                        for (idx, arg) in args.iter().enumerate() {
                            if idx > 0 {
                                self.diagnostics.add(
                                    DiagnosticKind::Error,
                                    *span,
                                    format!(
                                        "a call to {} requires exactly one argument",
                                        procedure_name
                                    ),
                                );
                                break;
                            }
                            match arg.get() {
                                ast::Expr::Variable(_) => {
                                    let ty = self.ctx.get_ast_type(arg.id()).unwrap();
                                    if !self.ctx.type_system.is_pointer_type(ty) {
                                        self.diagnostics.add(DiagnosticKind::Error, *span,
                                            format!("the argument to {} must be a variable of pointer type", procedure_name));
                                    }
                                }
                                _ => {
                                    self.diagnostics.add(
                                        DiagnosticKind::Error,
                                        *arg.loc(),
                                        format!("must be a variable"),
                                    );
                                }
                            }
                        }
                    } else {
                        self.diagnostics.add(
                            DiagnosticKind::Error,
                            *span,
                            format!("a call to {} requires exactly one argument", procedure_name),
                        );
                    }
                }
                "rewrite" | "reset" | "put" | "get" => {
                    if let Some(args) = &node.1 {
                        for (idx, arg) in args.iter().enumerate() {
                            if idx > 0 {
                                self.diagnostics.add(
                                    DiagnosticKind::Error,
                                    *span,
                                    format!(
                                        "a call to {} requires exactly one argument",
                                        procedure_name
                                    ),
                                );
                                break;
                            }
                            let ty = self.ctx.get_ast_type(arg.id()).unwrap();
                            if !self.ctx.type_system.is_file_type(ty) {
                                self.diagnostics.add(
                                    DiagnosticKind::Error,
                                    *span,
                                    format!(
                                        "the argument to {} must be a variable of file type",
                                        procedure_name
                                    ),
                                );
                            }
                        }
                    } else {
                        self.diagnostics.add(
                            DiagnosticKind::Error,
                            *span,
                            format!("a call to {} requires exactly one argument", procedure_name),
                        );
                    }
                }
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

        false
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
        id: span::SpanId,
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
        let ty = if must_be_real && !self.ctx.type_system.is_real_type(ty) {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *node.0.loc(),
                format!("argument must be of real type because frac-width was specified",),
            );
            self.ctx.type_system.get_error_type()
        } else if !self.ctx.type_system.is_real_type(ty)
            && !self.ctx.type_system.is_bool_type(ty)
            && !self.ctx.type_system.is_integer_type(ty)
        {
            self.diagnostics.add(
                    DiagnosticKind::Error,
                    *node.0.loc(),
                    format!("argument of writeln must be an expression of integer-type, real-type or Boolean-type"),
                );
            self.ctx.type_system.get_error_type()
        } else {
            ty
        };
        self.ctx.set_ast_type(id, ty);
    }

    fn visit_post_procedure_and_function_declaration_part(
        &mut self,
        n: &mut ast::ProcedureAndFunctionDeclarationPart,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        // Propagate the required symbols to the enclosing procedure/function (if any).
        let current_scope_symbol = self
            .ctx
            .scope
            .get_innermost_scope_symbol(self.ctx.scope.get_current_scope_id());
        if current_scope_symbol.is_none() {
            return;
        }
        let current_scope_symbol = current_scope_symbol.unwrap();

        let mut items: HashSet<SymbolId> = HashSet::new();

        for decl in &n.0 {
            let decl = decl.get();
            match decl {
                ast::ProcedureAndFunctionDeclaration::Procedure(proc) => {
                    let proc = proc.0.get();
                    match proc {
                        ast::ProcedureDeclaration::Definition(proc) => {
                            let proc_sym = self.ctx.get_ast_symbol(proc.0.id()).unwrap();
                            let proc_sym = self.ctx.get_symbol(proc_sym);
                            let proc_sym = proc_sym.borrow();
                            proc_sym.get_required_environment().iter().for_each(|item| {
                                items.insert(*item);
                            });
                        }
                        ast::ProcedureDeclaration::Forward(_) => {}
                    }
                }
                ast::ProcedureAndFunctionDeclaration::Function(func) => {
                    let func = func.0.get();
                    match func {
                        ast::FunctionDeclaration::Definition(func) => {
                            let func_sym = self.ctx.get_ast_symbol(func.0.id()).unwrap();
                            let func_sym = self.ctx.get_symbol(func_sym);
                            let func_sym = func_sym.borrow();
                            func_sym.get_required_environment().iter().for_each(|item| {
                                items.insert(*item);
                            });
                        }
                        ast::FunctionDeclaration::LateDefinition(func) => {
                            let func_sym = self.ctx.get_ast_symbol(func.0.id()).unwrap();
                            let func_sym = self.ctx.get_symbol(func_sym);
                            let func_sym = func_sym.borrow();
                            func_sym.get_required_environment().iter().for_each(|item| {
                                items.insert(*item);
                            });
                        }
                        ast::FunctionDeclaration::Forward(_) => {}
                    }
                }
            }
        }

        let current_scope_symbol = self.ctx.get_symbol(current_scope_symbol);
        let mut current_scope_symbol = current_scope_symbol.borrow_mut();
        items.iter().for_each(|sym_id| {
            let sym = self.ctx.get_symbol(*sym_id);
            let sym = sym.borrow();
            let sym_scope = sym.get_scope().unwrap();
            if self
                .ctx
                .scope
                .scope_is_same_or_nested_in(self.ctx.scope.get_current_scope_id(), sym_scope)
            {
                current_scope_symbol.add_to_required_environment(*sym_id);
            }
        });
    }

    fn visit_pre_function_definition(
        &mut self,
        n: &mut ast::FunctionDefinition,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        let function_name = n.0.get();

        let redeclaration_status =
            self.diagnose_reintroduced_function_definition(function_name, n.0.loc());
        match redeclaration_status {
            FunctionProcedureDeclarationStatus::AlreadyDeclared(_)
            | FunctionProcedureDeclarationStatus::AlreadyDefined(_) => {
                // This is an error.
                return false;
            }
            FunctionProcedureDeclarationStatus::ForwardDeclared(_) => {
                // Fine but check for equivalence.
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

        let function_decl_scope_id = self.ctx.scope.get_current_scope_id();
        self.ctx.scope.push_scope(None);

        let formal_parameters = self.gather_formal_parameters(&mut n.1);
        // Function definitions always must have parameters. Late ones may not, but these are handled elsewhere.
        let formal_parameters = formal_parameters.unwrap_or_else(|| vec![]);
        let result_type = self.gather_return_type(&mut n.2);

        let function_sym_id = match redeclaration_status {
            FunctionProcedureDeclarationStatus::ForwardDeclared(prev_sym_id) => {
                if !self.equivalent_function_declarations(
                    prev_sym_id,
                    &formal_parameters,
                    result_type,
                ) {
                    let prev_sym = self.ctx.get_symbol(prev_sym_id);
                    let prev_sym = prev_sym.borrow();
                    self.diagnostics.add_with_extra(DiagnosticKind::Error, *n.0.loc(),
                    format!("function definition is incompatible with a previous function declaration"),
                     vec![],
                     vec![Diagnostic::new(DiagnosticKind::Info, prev_sym.get_defining_point().unwrap(), format!("previous declaration"))]);
                    None
                } else {
                    let prev_sym = self.ctx.get_symbol(prev_sym_id);
                    let mut prev_sym = prev_sym.borrow_mut();
                    prev_sym.set_defined(true);
                    prev_sym.set_defining_point(*n.0.loc());
                    prev_sym.set_formal_parameters(formal_parameters);

                    let return_sym_id = prev_sym.get_return_symbol().unwrap();
                    let return_sym = self.ctx.get_symbol(return_sym_id);
                    let mut return_sym = return_sym.borrow_mut();
                    return_sym.set_defined(true);
                    return_sym.set_defining_point(*n.0.loc());

                    Some(prev_sym_id)
                }
            }
            FunctionProcedureDeclarationStatus::NotDeclared => {
                // Fine
                Some(self.create_new_function_symbol(
                    function_decl_scope_id,
                    function_name,
                    formal_parameters,
                    result_type,
                    /* is_definition */ true,
                    *n.0.loc(),
                ))
            }
            _ => {
                unreachable!();
            }
        };

        if function_sym_id.is_none() {
            self.ctx.scope.pop_scope();
            return false;
        }

        let function_sym_id = function_sym_id.unwrap();

        self.ctx.scope.set_scope_symbol(Some(function_sym_id));
        self.ctx.set_ast_symbol(n.0.id(), function_sym_id);

        let function_block = &mut n.3;
        let loc = *function_block.loc();
        let id = function_block.id();
        function_block.get_mut().mutating_walk_mut(self, &loc, id);

        self.ctx.scope.pop_scope();

        false
    }

    fn visit_pre_procedure_definition(
        &mut self,
        n: &mut ast::ProcedureDefinition,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        let proc_name = n.0.get();

        let redeclaration_status =
            self.diagnose_reintroduced_procedure_definition(proc_name, n.0.loc());
        match redeclaration_status {
            FunctionProcedureDeclarationStatus::AlreadyDeclared(_)
            | FunctionProcedureDeclarationStatus::AlreadyDefined(_) => {
                return false;
            }
            FunctionProcedureDeclarationStatus::ForwardDeclared(_) => {
                // Fine but check for equivalence.
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

        let proc_decl_scope_id = self.ctx.scope.get_current_scope_id();

        // We will set the scope type later, once we know the exact symbol we want to use.
        self.ctx.scope.push_scope(None);

        // This registers the parameters (if any) in the current scope.
        let formal_parameters = self.gather_formal_parameters(&mut n.1);

        let proc_sym_id = match redeclaration_status {
            FunctionProcedureDeclarationStatus::ForwardDeclared(prev_sym_id) => {
                // If there are no formal parameters at all we will use those of the prev_sym.
                if let Some(formal_parameters) = formal_parameters {
                    if !self.equivalent_procedure_declarations(prev_sym_id, &formal_parameters) {
                        let prev_sym = self.ctx.get_symbol(prev_sym_id);
                        let prev_sym = prev_sym.borrow();
                        self.diagnostics.add_with_extra(DiagnosticKind::Error, *n.0.loc(),
                    format!("procedure definition is incompatible with a previous procedure declaration"),
                     vec![],
                     vec![Diagnostic::new(DiagnosticKind::Info, prev_sym.get_defining_point().unwrap(), format!("previous declaration"))]);
                        None
                    } else {
                        let prev_sym = self.ctx.get_symbol(prev_sym_id);
                        let mut prev_sym = prev_sym.borrow_mut();
                        prev_sym.set_formal_parameters(formal_parameters);
                        Some(prev_sym_id)
                    }
                } else {
                    let prev_sym = self.ctx.get_symbol(prev_sym_id);
                    let prev_sym = prev_sym.borrow();

                    let prev_formal_parameters = prev_sym.get_formal_parameters();
                    // Insert the formal parameters in the scope, so name resolution works.
                    if let Some(prev_formal_parameters) = prev_formal_parameters {
                        for prev_formal_param_sym_id in prev_formal_parameters.iter().flatten().cloned() {
                            let prev_formal_param = self.ctx.get_symbol(prev_formal_param_sym_id);
                            let mut prev_formal_param = prev_formal_param.borrow_mut();
                            prev_formal_param.set_scope(self.ctx.scope.get_current_scope_id());
                            self.ctx.scope
                                .add_entry(&prev_formal_param.get_name(), prev_formal_param_sym_id);
                        }
                    }
                    Some(prev_sym_id)
                }.map(|prev_sym_id| {
                    let prev_sym = self.ctx.get_symbol(prev_sym_id);
                    let mut prev_sym = prev_sym.borrow_mut();
                    prev_sym.set_defined(true);
                    prev_sym.set_defining_point(*n.0.loc());
                    prev_sym_id
                })
            }
            FunctionProcedureDeclarationStatus::NotDeclared => {
                let formal_parameters = formal_parameters.unwrap_or_else(|| vec![]);
                Some(self.create_new_procedure_symbol(
                    proc_decl_scope_id,
                    proc_name,
                    formal_parameters,
                    /* is_definition */ true,
                    *n.0.loc(),
                ))
            }
            _ => {
                panic!("Unexpected case");
            }
        };

        if proc_sym_id.is_none() {
            self.ctx.scope.pop_scope();
            return false;
        }

        let proc_sym_id = proc_sym_id.unwrap();

        self.ctx.scope.set_scope_symbol(Some(proc_sym_id));
        self.ctx.set_ast_symbol(n.0.id(), proc_sym_id);

        let proc_block = &mut n.2;
        let loc = *proc_block.loc();
        let id = proc_block.id();
        proc_block.get_mut().mutating_walk_mut(self, &loc, id);

        self.ctx.scope.pop_scope();

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
        let param_ty = if self
            .ctx
            .type_system
            .is_valid_component_type_of_file_type(param_ty)
        {
            param_ty
        } else {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *type_name.loc(),
                format!(
                    "type {} cannot be used for a value parameter",
                    self.ctx.type_system.get_type_name(param_ty)
                ),
            );
            self.ctx.type_system.get_error_type()
        };

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

    fn visit_post_index_type_specification(
        &mut self,
        n: &mut ast::IndexTypeSpecification,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        let lower = &n.0;
        let upper = &n.1;
        let type_specifier = &n.2;

        let type_id = self.ctx.get_ast_type(type_specifier.id()).unwrap();
        let type_id = if !self.ctx.type_system.is_ordinal_type(type_id)
            && !self.ctx.type_system.is_error_type(type_id)
        {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *type_specifier.loc(),
                format!("type must be an ordinal type"),
            );
            self.ctx.type_system.get_error_type()
        } else {
            type_id
        };

        let declare_bound = |bound: &span::Spanned<String>, self_: &mut Self| {
            let name = bound.get();
            let id = bound.id();
            let loc = bound.loc();

            let mut new_sym = Symbol::new();
            new_sym.set_name(name);
            new_sym.set_kind(SymbolKind::BoundIdentifier);
            new_sym.set_defining_point(*loc);
            new_sym.set_type(type_id);
            new_sym.set_scope(self_.ctx.scope.get_current_scope_id());

            let new_sym = self_.ctx.new_symbol(new_sym);
            self_.ctx.scope.add_entry(name, new_sym);
            self_.ctx.set_ast_symbol(id, new_sym);
        };

        if !self.diagnose_redeclared_symbol(lower.get(), lower.loc()) {
            declare_bound(lower, self);
        }
        if !self.diagnose_redeclared_symbol(upper.get(), upper.loc()) {
            declare_bound(upper, self);
        }
    }

    fn visit_pre_conformable_array_schema(
        &mut self,
        n: &mut ast::ConformableArraySchema,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        // We visit these in preorder because they are declarations.
        n.1.iter_mut().for_each(|x| {
            let loc = *x.loc();
            let id = x.id();
            x.get_mut().mutating_walk_mut(self, &loc, id)
        });

        {
            let loc = *n.2.loc();
            let id = n.2.id();
            n.2.get_mut().mutating_walk_mut(self, &loc, id);
        }

        // Now synthesise the type.
        let component_type = &n.2;
        let component_type_id = self.ctx.get_ast_type(component_type.id()).unwrap();

        let index_type_specs = &n.1;
        assert!(!index_type_specs.is_empty());

        let packed = n.0.is_some();

        let mut array_type = component_type_id;

        for index_type_spec in index_type_specs.iter().rev() {
            let lower = &index_type_spec.get().0;
            let lower = self.ctx.get_ast_symbol(lower.id()).unwrap();

            let upper = &index_type_spec.get().1;
            let upper = self.ctx.get_ast_symbol(upper.id()).unwrap();

            let mut new_array = Type::default();
            new_array.set_kind(TypeKind::ConformableArray {
                packed,
                lower,
                upper,
                component: array_type,
            });

            array_type = self.ctx.type_system.new_type(new_array);
        }

        self.ctx.set_ast_type(id, array_type);

        false
    }

    fn visit_post_array_schema(
        &mut self,
        n: &mut ast::ArraySchema,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let ty = self.ctx.get_ast_type(n.0.id()).unwrap();
        self.ctx.set_ast_type(id, ty);
    }

    fn visit_post_formal_parameter_value_conformable_array(
        &mut self,
        n: &mut ast::FormalParameterValueConformableArray,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        let names = &n.0;
        let type_name = &n.1;

        let param_ty = self.ctx.get_ast_type(type_name.id()).unwrap();

        names.iter().for_each(|name| {
            self.declare_formal_parameter(name, param_ty, ParameterKind::ValueConformableArray);
        });
    }

    fn visit_post_formal_parameter_variable_conformable_array(
        &mut self,
        n: &mut ast::FormalParameterVariableConformableArray,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        let names = &n.0;
        let type_name = &n.1;

        let param_ty = self.ctx.get_ast_type(type_name.id()).unwrap();

        names.iter().for_each(|name| {
            self.declare_formal_parameter(name, param_ty, ParameterKind::VariableConformableArray);
        });
    }

    fn visit_pre_formal_parameter_function(
        &mut self,
        _n: &mut ast::FormalParameterFunction,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        self.ctx.scope.push_scope(None);
        true
    }

    fn visit_post_formal_parameter_function(
        &mut self,
        n: &mut ast::FormalParameterFunction,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        self.ctx.scope.pop_scope();

        let name = &n.0;
        let formal_parameters =
            n.1.as_ref()
                .unwrap_or(&vec![])
                .iter()
                .map(|x| self.get_symbols_formal_parameter(x))
                .collect::<Vec<Vec<_>>>();
        let result_type = self.ctx.get_ast_type(n.2.id()).unwrap();

        self.declare_function_parameter(name, formal_parameters, result_type);
    }

    fn visit_pre_formal_parameter_procedure(
        &mut self,
        _n: &mut ast::FormalParameterProcedure,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        self.ctx.scope.push_scope(None);
        true
    }

    fn visit_post_formal_parameter_procedure(
        &mut self,
        n: &mut ast::FormalParameterProcedure,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) {
        self.ctx.scope.pop_scope();

        let name = &n.0;
        let formal_parameters =
            n.1.as_ref()
                .unwrap_or(&vec![])
                .iter()
                .map(|x| self.get_symbols_formal_parameter(x))
                .collect::<Vec<Vec<_>>>();

        self.declare_procedure_parameter(name, formal_parameters);
    }
}

pub fn check_program(
    program: &mut span::SpannedBox<ast::Program>,
    semantic_context: &mut SemanticContext,
    diagnostics: &mut Diagnostics,
) {
    // Init global scope.
    semantic_context.init_global_scope();

    let mut checker_visitor = SemanticCheckerVisitor {
        ctx: semantic_context,
        diagnostics,
        in_type_definition_part: false,
        in_pointer_type: false,
        program_heading_loc: None,
        record_info: typesystem::VariantPart::default(),
    };

    let loc = *program.loc();
    let id = program.id();
    program
        .get_mut()
        .mutating_walk_mut(&mut checker_visitor, &loc, id);
}

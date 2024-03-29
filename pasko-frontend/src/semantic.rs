use crate::ast::{self, ExprVariableReference, UnaryOp};
use crate::ast::{BinOperand, FormalParameter};
use crate::constant::Constant;
use crate::diagnostics::{Diagnostic, DiagnosticKind, Diagnostics};
use crate::scope;
use crate::span;
use crate::symbol::{ParameterKind, Symbol, SymbolId, SymbolKind};
use crate::typesystem::{Type, TypeId, TypeKind};
use crate::visitor::MutatingVisitable;
use crate::visitor::MutatingVisitorMut;

use std::collections::{HashMap, HashSet};

use std::rc::Rc;

pub struct SemanticContext {
    types: HashMap<TypeId, Rc<Type>>,
    derived_types: HashSet<Rc<Type>>,
    symbols: HashMap<SymbolId, Symbol>,

    ast_types: HashMap<span::SpanId, TypeId>,
    ast_symbols: HashMap<span::SpanId, SymbolId>,
    ast_values: HashMap<span::SpanId, Constant>,

    integer_type_id: TypeId,
    real_type_id: TypeId,
    bool_type_id: TypeId,
    error_type_id: TypeId,
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

impl SemanticContext {
    pub fn new() -> SemanticContext {
        let mut types = HashMap::new();

        let mut integer_type = Type::default();
        integer_type.set_kind(TypeKind::Integer);
        let integer_type_id = integer_type.id();
        types.insert(integer_type_id, Rc::new(integer_type));

        let mut real_type = Type::default();
        real_type.set_kind(TypeKind::Real);
        let real_type_id = real_type.id();
        types.insert(real_type_id, Rc::new(real_type));

        let mut bool_type = Type::default();
        bool_type.set_kind(TypeKind::Bool);
        let bool_type_id = bool_type.id();
        types.insert(bool_type_id, Rc::new(bool_type));

        let mut error_type = Type::default();
        error_type.set_kind(TypeKind::Error);
        let error_type_id = error_type.id();
        types.insert(error_type_id, Rc::new(error_type));

        SemanticContext {
            types,
            derived_types: HashSet::new(),
            symbols: HashMap::new(),

            ast_types: HashMap::new(),
            ast_symbols: HashMap::new(),
            ast_values: HashMap::new(),

            integer_type_id,
            real_type_id,
            bool_type_id,
            error_type_id,
        }
    }

    pub fn new_type(&mut self, ty: Type) -> TypeId {
        if let Some(ty) = self.types.get(&ty.id()) {
            return ty.id();
        }
        let new_id = ty.id();
        self.types.insert(new_id, Rc::new(ty));
        new_id
    }

    fn get_type_internal(&self, id: TypeId) -> &Type {
        self.types.get(&id).unwrap()
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
        if let Some(sym) = self.symbols.get(&sym.id()) {
            return sym.id();
        }
        let new_id = sym.id();
        self.symbols.insert(new_id, sym);
        new_id
    }

    pub fn get_symbol(&self, id: SymbolId) -> &Symbol {
        self.symbols.get(&id).unwrap()
    }

    pub fn get_symbol_mut(&mut self, id: SymbolId) -> &mut Symbol {
        self.symbols.get_mut(&id).unwrap()
    }

    pub fn set_ast_symbol(&mut self, id: span::SpanId, sym: SymbolId) {
        self.ast_symbols.insert(id, sym);
    }

    pub fn get_ast_symbol(&self, id: span::SpanId) -> Option<SymbolId> {
        self.ast_symbols.get(&id).cloned()
    }

    pub fn get_integer_type(&self) -> TypeId {
        self.integer_type_id
    }

    pub fn is_integer_type(&self, ty: TypeId) -> bool {
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        ty.is_integer_type()
    }

    pub fn get_real_type(&self) -> TypeId {
        self.real_type_id
    }

    pub fn is_real_type(&self, ty: TypeId) -> bool {
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        ty.is_real_type()
    }

    pub fn get_bool_type(&self) -> TypeId {
        self.bool_type_id
    }

    pub fn is_bool_type(&self, ty: TypeId) -> bool {
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        ty.is_bool_type()
    }

    pub fn get_error_type(&self) -> TypeId {
        self.error_type_id
    }

    pub fn is_error_type(&self, ty: TypeId) -> bool {
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        ty.is_error_type()
    }

    pub fn get_string_type(&mut self, len: usize) -> TypeId {
        let mut new_string_type = Type::default();
        new_string_type.set_kind(TypeKind::String(len));

        let new_string_type = Rc::new(new_string_type);
        match self.derived_types.get(&new_string_type.clone()) {
            Some(x) => return x.id(),
            _ => {}
        }

        let new_id = new_string_type.id();

        self.derived_types.insert(new_string_type.clone());
        self.types.insert(new_id, new_string_type);

        new_id
    }

    pub fn is_string_type(&self, ty: TypeId) -> bool {
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);
        return ty.is_string_type();
    }

    pub fn get_type_name(&self, id: TypeId) -> String {
        let ty = self.get_type_internal(id);
        match ty.get_kind() {
            TypeKind::NamedType(sym_id) => {
                let sym = self.get_symbol(sym_id);
                assert!(!self.is_builtin_type_name(sym.get_name()));
                sym.get_name().clone()
            }
            TypeKind::Integer => "integer".to_string(),
            TypeKind::Real => "real".to_string(),
            TypeKind::Bool => "boolean".to_string(),
            TypeKind::String(len) => format!("string of {} characters", len),
            _ => {
                unreachable!("Cannot print name of type {:?}", ty.get_kind());
            }
        }
    }

    pub fn is_builtin_type_name(&self, name: &str) -> bool {
        match name {
            "integer" | "real" | "boolean" => true,
            _ => false,
        }
    }

    fn ultimate_type(&self, ty: TypeId) -> TypeId {
        let lhs_type = self.get_type_internal(ty);
        if let Some(sym_id) = lhs_type.get_symbol_of_named_type() {
            let sym = self.get_symbol(sym_id);
            return self.ultimate_type(sym.get_type().unwrap());
        }
        ty
    }

    pub fn same_type(&self, a: TypeId, b: TypeId) -> bool {
        let a = self.ultimate_type(a);
        let b = self.ultimate_type(b);

        a == b
    }

    pub fn is_valid_component_type_of_file_type(&self, _ty: TypeId) -> bool {
        // TODO:
        true
    }

    pub fn is_simple_type(&self, ty: TypeId) -> bool {
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        ty.is_simple_type()
    }

    pub fn is_ordinal_type(&self, ty: TypeId) -> bool {
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        ty.is_ordinal_type()
    }

    pub fn size_in_bytes(&self, ty: TypeId) -> usize {
        if self.is_real_type(ty) || self.is_integer_type(ty) {
            8
        } else if self.is_bool_type(ty) {
            1
        } else {
            panic!(
                "Unexpected size request for type '{}'",
                self.get_type_name(ty)
            );
        }
    }
}

struct SemanticCheckerVisitor<'a> {
    ctx: &'a mut SemanticContext,
    diagnostics: &'a mut Diagnostics,
    scope: &'a mut scope::Scope,
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

    fn is_compatible(&self, lhs_type_id: TypeId, rhs_type_id: TypeId) -> bool {
        if self.ctx.same_type(lhs_type_id, rhs_type_id) {
            return true;
        }

        // TODO: lhs is a subrange of rhs, or rhs is a subrange of lhs, or both lhs and rhs are subranges of the same host-type.

        // TODO: lhs and rhs are set-types of compatible base-types, and either both lhs and rhs are designated packed or neither lhs nor rhs is designated packed.

        // TODO: lhs and rhs are string-types with the same number of components

        false
    }

    fn is_assignment_compatible(&self, lhs_type_id: TypeId, rhs_type_id: TypeId) -> bool {
        if self.ctx.same_type(lhs_type_id, rhs_type_id) {
            return self.ctx.is_valid_component_type_of_file_type(lhs_type_id);
        }

        if self.ctx.is_real_type(lhs_type_id) && self.ctx.is_integer_type(rhs_type_id) {
            return true;
        }

        // TODO: types are compatible ordinal types and the value of rhs is in the closed interval of lhs

        // TODO: types are compatible set types and all the members of rhs are in the closed interval specified by lhs

        // TODO: lhs and rhs are compatible string types

        false
    }

    fn common_arith_type(&self, lhs_ty: TypeId, rhs_ty: TypeId) -> Option<TypeId> {
        if self.ctx.is_integer_type(lhs_ty) && self.ctx.is_integer_type(rhs_ty) {
            return Some(self.ctx.get_integer_type());
        }

        if self.ctx.is_real_type(lhs_ty) && self.ctx.is_real_type(rhs_ty)
            || self.ctx.is_integer_type(lhs_ty) && self.ctx.is_real_type(rhs_ty)
            || self.ctx.is_real_type(lhs_ty) && self.ctx.is_integer_type(rhs_ty)
        {
            return Some(self.ctx.get_real_type());
        }
        None
    }

    fn real_arith_type(&self, lhs_ty: TypeId, rhs_ty: TypeId) -> Option<TypeId> {
        if self.ctx.is_integer_type(lhs_ty) && self.ctx.is_integer_type(rhs_ty)
            || self.ctx.is_real_type(lhs_ty) && self.ctx.is_real_type(rhs_ty)
            || self.ctx.is_integer_type(lhs_ty) && self.ctx.is_real_type(rhs_ty)
            || self.ctx.is_real_type(lhs_ty) && self.ctx.is_integer_type(rhs_ty)
        {
            return Some(self.ctx.get_real_type());
        }
        None
    }

    fn both_integer_type(&self, lhs_ty: TypeId, rhs_ty: TypeId) -> Option<TypeId> {
        if self.ctx.is_integer_type(lhs_ty) && self.ctx.is_integer_type(rhs_ty) {
            return Some(self.ctx.get_integer_type());
        }
        None
    }

    fn both_bool_type(&self, lhs_ty: TypeId, rhs_ty: TypeId) -> Option<TypeId> {
        if self.ctx.is_bool_type(lhs_ty) && self.ctx.is_bool_type(rhs_ty) {
            return Some(self.ctx.get_bool_type());
        }
        None
    }

    fn relational_compatibility(&self, lhs_type_id: TypeId, rhs_type_id: TypeId) -> Option<TypeId> {
        // TODO: set-types
        if self.is_compatible(lhs_type_id, rhs_type_id) {
            return Some(lhs_type_id);
        } else if self.ctx.is_integer_type(lhs_type_id) && self.ctx.is_real_type(rhs_type_id) {
            return Some(rhs_type_id);
        } else if self.ctx.is_real_type(lhs_type_id) || self.ctx.is_integer_type(rhs_type_id) {
            return Some(lhs_type_id);
        }

        None
    }

    fn valid_for_relational(&self, lhs_type_id: TypeId, rhs_type_id: TypeId) -> Option<TypeId> {
        let valid_type = |ty: TypeId| {
            self.ctx.is_simple_type(ty) // || ty.is_string_type()
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
            self.ctx.is_simple_type(ty) // || ty.is_string_type()
        };

        if !valid_type(lhs_type_id) || !valid_type(rhs_type_id) {
            return None;
        }

        self.relational_compatibility(lhs_type_id, rhs_type_id)
    }

    fn valid_for_equality(&self, lhs_type_id: TypeId, rhs_type_id: TypeId) -> Option<TypeId> {
        let valid_type = |ty: TypeId| {
            self.ctx.is_simple_type(ty) // || ty.is_string_type()
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
        self.ctx.set_ast_type(id, self.ctx.get_error_type());
        self.diagnostics.add_with_extra(
            DiagnosticKind::Error,
            operator_loc,
            format!(
                "operator '{}' cannot be applied to operands of type '{}' and '{}'",
                operator_name,
                self.ctx.get_type_name(lhs_ty),
                self.ctx.get_type_name(rhs_ty)
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
        self.ctx.set_ast_type(id, self.ctx.get_error_type());
        self.diagnostics.add_with_extra(
            DiagnosticKind::Error,
            operator_loc,
            format!(
                "operator '{}' cannot be applied to operand of type '{}'",
                operator_name,
                self.ctx.get_type_name(op_ty)
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
        if arguments.iter().any(|arg| self.ctx.is_error_type(arg.2)) {
            return false;
        }

        let params = {
            let callee_symbol = self.ctx.get_symbol(callee_symbol_id);
            callee_symbol.get_formal_parameters().unwrap()
        };

        // If any parameter of the function declaration was already wrong, give up.
        if params.iter().any(|param| {
            let param_sym = self.ctx.get_symbol(*param);
            self.ctx.is_error_type(param_sym.get_type().unwrap())
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
                                                    self.ctx.get_type_name(arg.2),
                                                    param_sym.get_name(),
                                                    self.ctx.get_type_name(param_type_id)
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
                    } else if param_type_id != arg.2 {
                        self.diagnostics.add(
                                                DiagnosticKind::Error,
                                                arg.1,
                                                format!(
                                                    "argument has type {} but it is different to variable parameter '{}' of type {}",
                                                    self.ctx.get_type_name(arg.2),
                                                    param_sym.get_name(),
                                                    self.ctx.get_type_name(param_type_id)
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

        let sym = self.scope.lookup_current_scope(name);

        match sym {
            Some(x) => {
                let extra = self.extra_diag_previous_location(self.ctx.get_symbol(x));
                self.diagnostics.add_with_extra(
                    DiagnosticKind::Error,
                    *n.0.loc(),
                    format!(
                        "identifier '{}' has already been declared in this scope",
                        n.0.get()
                    ),
                    vec![],
                    extra,
                );
                return;
            }
            None => {}
        }

        let mut new_sym = Symbol::new();
        new_sym.set_name(name);
        new_sym.set_kind(SymbolKind::Const);
        new_sym.set_defining_point(*n.0.loc());
        new_sym.set_type(const_ty);

        if let Some(val) = self.ctx.get_ast_value(n.1.id()) {
            new_sym.set_const(val);
        } else {
            assert!(self.ctx.is_error_type(const_ty));
        }

        let new_sym = self.ctx.new_symbol(new_sym);
        self.scope.add_entry(name, new_sym);
        self.ctx.set_ast_symbol(n.0.id(), new_sym);
    }

    fn visit_pre_type_definition_part(
        &mut self,
        _n: &mut ast::TypeDefinitionPart,
        span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        self.unimplemented(*span, "declaration of types");
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
            match type_name.get_kind() {
                SymbolKind::Type => {
                    let named_type = if self.ctx.is_builtin_type_name(type_name.get_name()) {
                        type_name.get_type().unwrap()
                    } else {
                        let mut new_type = Type::default();
                        new_type.set_kind(TypeKind::NamedType(type_name.id()));

                        let ty = self.ctx.new_type(new_type);
                        ty
                    };
                    self.ctx.set_ast_type(id, named_type);
                }
                SymbolKind::ErrorLookup => {
                    self.ctx.set_ast_type(id, self.ctx.get_error_type());
                }
                _ => {
                    let extra = self.extra_diag_previous_location(type_name);
                    self.ctx.set_ast_type(id, self.ctx.get_error_type());
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
            self.ctx.set_ast_type(id, self.ctx.get_error_type());
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
            let sym = self.scope.lookup_current_scope(e.get());
            match sym {
                Some(x) => {
                    let extra = self.extra_diag_previous_location(self.ctx.get_symbol(x));
                    self.diagnostics.add_with_extra(
                        DiagnosticKind::Error,
                        *e.loc(),
                        format!(
                            "identifier '{}' has already been declared in this scope",
                            e.get()
                        ),
                        vec![],
                        extra,
                    );
                    return;
                }
                None => {}
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
                (var_name.get_kind(), var_name.get_type())
            };

            match sym_kind {
                SymbolKind::Variable => {
                    self.ctx.set_ast_symbol(id, sym_id);
                    self.ctx.set_ast_type(id, sym_type.unwrap());
                }
                SymbolKind::ErrorLookup => {
                    self.ctx.set_ast_type(id, self.ctx.get_error_type());
                }
                _ => {
                    let extra = {
                        let var_name = self.ctx.get_symbol(sym_id);
                        self.extra_diag_previous_location(var_name)
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
                    self.ctx.set_ast_type(id, self.ctx.get_error_type());
                }
            }
        } else {
            // The error is already diagnosed during the lookup.
            self.ctx.set_ast_type(id, self.ctx.get_error_type());
        }
    }

    fn visit_pre_assig_array_access(
        &mut self,
        _n: &mut ast::AssigArrayAccess,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.unimplemented(*span, "array accesses");
        self.ctx.set_ast_type(id, self.ctx.get_error_type());
        false
    }

    fn visit_pre_assig_field_access(
        &mut self,
        _n: &mut ast::AssigFieldAccess,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.unimplemented(*span, "field accesses");
        self.ctx.set_ast_type(id, self.ctx.get_error_type());
        false
    }

    fn visit_pre_assig_pointer_deref(
        &mut self,
        _n: &mut ast::AssigPointerDeref,
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        self.unimplemented(*span, "pointer dereference");
        self.ctx.set_ast_type(id, self.ctx.get_error_type());
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
                                        assert!(self.ctx.is_error_type(sym_type.unwrap()));
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
                                            callee_symbol.get_return_symbol().unwrap()
                                        };
                                        let return_symbol = self.ctx.get_symbol(return_symbol_id);
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
        self.ctx.set_ast_type(id, self.ctx.get_error_type());
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
        span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        // Handle lhs or walk it.
        let mut must_walk_lhs = true;
        let lhs = &mut node.0;
        if let Some(scope_symbol_id) = self.scope.get_innermost_scope_symbol() {
            let scope_symbol = self.ctx.get_symbol(scope_symbol_id);
            if scope_symbol.get_kind() == SymbolKind::Function {
                match lhs.get_mut() {
                    ast::Assig::Variable(assig_var) => {
                        let name = assig_var.0.get();
                        if let Some(symbol_id) = self.scope.lookup(&name) {
                            if symbol_id == scope_symbol_id {
                                // This is an assignment to the return variable. Fixup.
                                let return_id = scope_symbol.get_return_symbol().unwrap();
                                let return_sym = self.ctx.get_symbol(return_id);
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
            if assig_sym.get_kind() != SymbolKind::Variable {
                self.diagnostics.add(
                    DiagnosticKind::Error,
                    *node.0.loc(),
                    format!("this is not a variable"),
                );
                self.ctx.set_ast_type(id, self.ctx.get_error_type());
                return false;
            }
        }

        let neither_is_err_type =
            { !self.ctx.is_error_type(lhs_type) && !self.ctx.is_error_type(rhs_type) };

        if neither_is_err_type {
            if self.is_assignment_compatible(lhs_type, rhs_type) {
                self.ctx.set_ast_type(id, lhs_type);
                if lhs_type != rhs_type {
                    let conversion = SemanticCheckerVisitor::create_conversion_expr(rhs.take());
                    rhs.reset(conversion);
                    self.ctx.set_ast_type(rhs.id(), lhs_type);
                }
                // FIXME: Conversions???
            } else {
                self.ctx.set_ast_type(id, self.ctx.get_error_type());
                let lhs_type_name = self.ctx.get_type_name(lhs_type);
                let rhs_type_name = self.ctx.get_type_name(rhs_type);
                self.diagnostics.add(DiagnosticKind::Error, *span, format!("left-hand side of this assignment has type '{}' that is not assignment-compatible with the type '{}' of the right-hand side", lhs_type_name, rhs_type_name));
            }
        } else {
            self.ctx.set_ast_type(id, self.ctx.get_error_type());
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

        if !self.ctx.is_bool_type(expr_ty) && !self.ctx.is_error_type(expr_ty) {
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
        if !self.ctx.is_error_type(var_ty_id) {
            if !self.ctx.is_ordinal_type(var_ty_id) {
                let sym_id = self.ctx.get_ast_symbol(node.1.id()).unwrap();
                let extra = {
                    let var_name = self.ctx.get_symbol(sym_id);
                    self.extra_diag_previous_location(var_name)
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

        if !self.ctx.is_bool_type(expr_ty) {
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

        if !self.ctx.is_bool_type(expr_ty) {
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

        if self.ctx.is_error_type(lhs_ty) || self.ctx.is_error_type(rhs_ty) {
            self.ctx.set_ast_type(id, self.ctx.get_error_type());
            return;
        }

        match node.0.get() {
            BinOperand::GreaterOrEqualThan | BinOperand::LowerOrEqualThan => {
                let op_type = self.valid_for_relational_equality(lhs_ty, rhs_ty);
                match op_type {
                    Some(operand_type) => {
                        if lhs_ty != operand_type {
                            assert!(
                                operand_type == rhs_ty,
                                "there is an extra conversion happening in the RHS?"
                            );
                            let conversion =
                                SemanticCheckerVisitor::create_conversion_expr(node.1.take());
                            node.1.reset(conversion);
                            self.ctx.set_ast_type(node.1.id(), operand_type);
                        } else if rhs_ty != operand_type {
                            let conversion =
                                SemanticCheckerVisitor::create_conversion_expr(node.2.take());
                            node.2.reset(conversion);
                            self.ctx.set_ast_type(node.2.id(), operand_type);
                        }
                        self.ctx.set_ast_type(id, self.ctx.get_bool_type());
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
                        if lhs_ty != operand_type {
                            assert!(
                                operand_type == rhs_ty,
                                "there is an extra conversion happening in the RHS?"
                            );
                            let conversion =
                                SemanticCheckerVisitor::create_conversion_expr(node.1.take());
                            node.1.reset(conversion);
                            self.ctx.set_ast_type(node.1.id(), operand_type);
                        } else if rhs_ty != operand_type {
                            let conversion =
                                SemanticCheckerVisitor::create_conversion_expr(node.2.take());
                            node.2.reset(conversion);
                            self.ctx.set_ast_type(node.2.id(), operand_type);
                        }
                        self.ctx.set_ast_type(id, self.ctx.get_bool_type());
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
                        if lhs_ty != operand_type {
                            assert!(
                                operand_type == rhs_ty,
                                "there is an extra conversion happening in the RHS?"
                            );
                            let conversion =
                                SemanticCheckerVisitor::create_conversion_expr(node.1.take());
                            node.1.reset(conversion);
                            self.ctx.set_ast_type(node.1.id(), operand_type);
                        } else if rhs_ty != operand_type {
                            let conversion =
                                SemanticCheckerVisitor::create_conversion_expr(node.2.take());
                            node.2.reset(conversion);
                            self.ctx.set_ast_type(node.2.id(), operand_type);
                        }
                        self.ctx.set_ast_type(id, self.ctx.get_bool_type());
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
                self.ctx.set_ast_type(id, self.ctx.get_error_type());
            }
            BinOperand::Addition | BinOperand::Subtraction | BinOperand::Multiplication => {
                let op_type = self.common_arith_type(lhs_ty, rhs_ty);
                match op_type {
                    Some(result_type) => {
                        if result_type != lhs_ty {
                            assert!(
                                result_type == rhs_ty,
                                "there is an extra conversion happening in the RHS?"
                            );
                            let conversion =
                                SemanticCheckerVisitor::create_conversion_expr(node.1.take());
                            node.1.reset(conversion);
                            self.ctx.set_ast_type(node.1.id(), result_type);
                        } else if result_type != rhs_ty {
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
                        if result_type != lhs_ty {
                            let conversion =
                                SemanticCheckerVisitor::create_conversion_expr(node.1.take());
                            node.1.reset(conversion);
                            self.ctx.set_ast_type(node.1.id(), result_type);
                        }
                        if result_type != rhs_ty {
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

        if self.ctx.is_error_type(op_ty_id) {
            self.ctx.set_ast_type(id, self.ctx.get_error_type());
            return;
        }

        match node.0.get() {
            UnaryOp::Negation | UnaryOp::Plus => {
                if self.ctx.is_integer_type(op_ty_id) || self.ctx.is_real_type(op_ty_id) {
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
                if self.ctx.is_bool_type(op_ty_id) {
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
                    self.ctx.set_ast_type(id, self.ctx.get_error_type());
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
            self.ctx.set_ast_type(id, self.ctx.get_error_type());
            return false;
        }

        if let Some(callee_symbol_id) = self.lookup_symbol(callee.get(), callee.loc()) {
            let callee_symbol_kind = self.ctx.get_symbol(callee_symbol_id).get_kind();
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
                        self.ctx.set_ast_type(id, self.ctx.get_error_type());
                        return false;
                    }

                    // All looks good, the expression has type the return type of the function.
                    self.ctx.set_ast_symbol(callee.id(), callee_symbol_id);

                    let return_symbol_id = {
                        let callee_symbol = self.ctx.get_symbol(callee_symbol_id);
                        callee_symbol.get_return_symbol().unwrap()
                    };

                    let return_symbol = self.ctx.get_symbol(return_symbol_id);
                    self.ctx.set_ast_type(id, return_symbol.get_type().unwrap());
                }
                SymbolKind::ErrorLookup => {
                    self.ctx.set_ast_type(id, self.ctx.get_error_type());
                }
                _ => {
                    let extra = {
                        let var_name = self.ctx.get_symbol(callee_symbol_id);
                        self.extra_diag_previous_location(var_name)
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
                    self.ctx.set_ast_type(id, self.ctx.get_error_type());
                }
            }
        } else {
            self.ctx.set_ast_type(id, self.ctx.get_error_type());
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
        self.ctx.set_ast_type(id, self.ctx.get_error_type());
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
        self.ctx.set_ast_type(id, self.ctx.get_integer_type());
        self.ctx.set_ast_value(id, Constant::Integer(*n.0.get()));
    }

    fn visit_const_real(
        &mut self,
        n: &mut ast::ConstReal,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        self.ctx.set_ast_type(id, self.ctx.get_real_type());
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
                    self.ctx.set_ast_type(id, self.ctx.get_error_type());
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
                    if !self.ctx.is_integer_type(sym_type.unwrap())
                        && !self.ctx.is_real_type(sym_type.unwrap())
                    {
                        self.diagnostics.add(
                            DiagnosticKind::Error,
                            *node.0.loc(),
                            "negation of a constant that is not an integer or a real".to_string(),
                        );
                        self.ctx.set_ast_type(id, self.ctx.get_error_type());
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
                    self.ctx.set_ast_type(id, self.ctx.get_error_type());
                }
                _ => {}
            }
        } else {
            self.ctx.set_ast_type(id, self.ctx.get_error_type());
        }
    }

    fn visit_const_string_literal(
        &mut self,
        n: &mut ast::ConstStringLiteral,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let str = n.0.get();
        let string_type = self.ctx.get_string_type(str.chars().count());
        self.ctx.set_ast_type(id, string_type);
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
        if !self.ctx.is_ordinal_type(expr_ty) {
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
                if const_ty != expr_ty && !case_expr_err {
                    self.diagnostics.add_with_extra(
                        DiagnosticKind::Error,
                        *case_const.loc(),
                        format!("constant of case must be of same ordinal type as case expression"),
                        vec![*case_expr.loc()],
                        vec![]
                    );
                }
                match self.ctx.get_ast_value(case_const.id()).unwrap() {
                    Constant::Integer(x) => { 
                        if let Some(prev_loc) = const_set.insert(x, case_const.loc()) {
                        let previous_const = Diagnostic::new(
                            DiagnosticKind::Info,
                            *prev_loc,
                            format!("previous case")
                        );
                        self.diagnostics.add_with_extra(
                            DiagnosticKind::Error,
                            *case_const.loc(),
                            format!("case repeated"),
                            vec![],
                            vec![previous_const]);
                        }
                    },
                    _ => { panic!("Unexpected constant"); }
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
            if self.ctx.is_error_type(type_id) {
                return;
            }

            if !self.ctx.is_integer_type(type_id) {
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
        if must_be_real && !self.ctx.is_real_type(ty) {
            self.diagnostics.add(
                DiagnosticKind::Error,
                *node.0.loc(),
                format!("argument must be of real type because frac-width was specified",),
            );
        } else if !self.ctx.is_real_type(ty)
            && !self.ctx.is_bool_type(ty)
            && !self.ctx.is_integer_type(ty)
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
                let callee_symbol_kind = self.ctx.get_symbol(callee_symbol_id).get_kind();
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
                            self.extra_diag_previous_location(var_name)
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

        if let Some(sym_id) = self.scope.lookup_current_scope(&function_name) {
            //
            let sym = self.ctx.get_symbol(sym_id);

            match sym.get_kind() {
                SymbolKind::Function => {
                    if sym.is_defined() {
                        let extra = self.extra_diag_previous_location(sym);
                        self.diagnostics.add_with_extra(
                            DiagnosticKind::Error,
                            *n.0.loc(),
                            format!("function '{}' has already been defined", n.0.get()),
                            vec![],
                            extra,
                        );
                    } else {
                        // FIXME: Check that the parameters and return match with an earlier declaration.
                    }
                }
                _ => {
                    // Already declared
                    let extra = self.extra_diag_previous_location(sym);
                    self.diagnostics.add_with_extra(
                        DiagnosticKind::Error,
                        *n.0.loc(),
                        format!(
                            "identifier '{}' has already been declared in this scope",
                            n.0.get()
                        ),
                        vec![],
                        extra,
                    );
                    // Do not attempt to semantically check anything else.
                    return false;
                }
            }
        } else if is_required_procedure_or_function(&function_name) {
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

        if let Some(sym_id) = self.scope.lookup_current_scope(&proc_name) {
            //
            let sym = self.ctx.get_symbol(sym_id);

            match sym.get_kind() {
                SymbolKind::Procedure => {
                    if sym.is_defined() {
                        let extra = self.extra_diag_previous_location(sym);
                        self.diagnostics.add_with_extra(
                            DiagnosticKind::Error,
                            *n.0.loc(),
                            format!("procedure '{}' has already been defined", n.0.get()),
                            vec![],
                            extra,
                        );
                    } else {
                        // FIXME: Check that the parameters match with an earlier declaration.
                    }
                }
                _ => {
                    // Already declared
                    let extra = self.extra_diag_previous_location(sym);
                    self.diagnostics.add_with_extra(
                        DiagnosticKind::Error,
                        *n.0.loc(),
                        format!(
                            "identifier '{}' has already been declared in this scope",
                            n.0.get()
                        ),
                        vec![],
                        extra,
                    );
                    // Do not attempt to semantically check anything else.
                    return false;
                }
            }
        } else if is_required_procedure_or_function(&proc_name) {
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
    new_sym.set_type(semantic_context.get_integer_type());
    let new_sym = semantic_context.new_symbol(new_sym);
    scope.add_entry("integer", new_sym);

    let mut new_sym = Symbol::new();
    new_sym.set_name("real");
    new_sym.set_kind(SymbolKind::Type);
    new_sym.set_type(semantic_context.get_real_type());
    let new_sym = semantic_context.new_symbol(new_sym);
    scope.add_entry("real", new_sym);

    let mut new_sym = Symbol::new();
    new_sym.set_name("boolean");
    new_sym.set_kind(SymbolKind::Type);
    new_sym.set_type(semantic_context.get_bool_type());
    let new_sym = semantic_context.new_symbol(new_sym);
    scope.add_entry("boolean", new_sym);

    let mut new_sym = Symbol::new();
    new_sym.set_name("true");
    new_sym.set_kind(SymbolKind::Const);
    new_sym.set_type(semantic_context.get_bool_type());
    new_sym.set_const(Constant::Bool(true));
    let new_sym = semantic_context.new_symbol(new_sym);
    scope.add_entry("true", new_sym);

    let mut new_sym = Symbol::new();
    new_sym.set_name("false");
    new_sym.set_kind(SymbolKind::Const);
    new_sym.set_type(semantic_context.get_bool_type());
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

use crate::constant::Constant;
use crate::scope;
use crate::span;
use crate::typesystem::TypeId;
use std::collections::HashSet;

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy, Ord, PartialOrd)]
pub struct SymbolId(usize);

impl From<SymbolId> for usize {
    fn from(id: SymbolId) -> usize {
        id.0
    }
}

impl Default for SymbolId {
    fn default() -> SymbolId {
        SymbolId(usize::MAX)
    }
}

impl SymbolId {
    pub fn get_id(&self) -> usize {
        // FIXME: get number is only for debugging.
        self.0
    }

    // Use only for debugging.
    pub fn build_from_id(id: usize) -> SymbolId {
        SymbolId(id)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    #[default]
    None,
    ErrorLookup,
    PendingTypeDefinition,
    Variable,
    Type,
    Function,
    Procedure,
    Const,
    Field,
    BoundIdentifier,
    Label,
    AssociatedField, // With statement
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParameterKind {
    Value,
    Variable,
    Procedure,
    Function,
    ValueConformableArray,
    VariableConformableArray,
}

#[derive(Debug, Default)]
struct SymbolInfo {
    name: String,
    kind: SymbolKind,
    ty: Option<TypeId>,
    def_loc: Option<span::SpanLoc>,
    scope: Option<scope::ScopeId>,
    val: Option<Constant>,
    defined: bool,
    required: bool,
    captured: bool,
    // FIXME: We can reduce the memory used by this by grouping these less
    // common things by the kind of Symbol.
    parameter: Option<ParameterKind>,
    // Used only for functions or procedures.
    formal_parameters: Option<Vec<Vec<SymbolId>>>,
    required_environment: HashSet<SymbolId>,
    // Used only for functions.
    return_symbol: Option<SymbolId>,
    // Used only for Fields
    associated_record_type: Option<TypeId>,
    // This is only for AssociatedFields
    associated_record: Option<span::SpanId>, // This is an ExprVariable
    associated_field: Option<SymbolId>,
}

#[derive(Debug)]
pub struct Symbol {
    id: SymbolId,
    info: SymbolInfo,
}

impl Default for Symbol {
    fn default() -> Self {
        Self::new()
    }
}

impl Symbol {
    pub fn new() -> Symbol {
        let mut sym = Symbol {
            id: SymbolId::default(),
            info: SymbolInfo::default(),
        };
        sym.info.defined = true;

        sym
    }
}

impl Symbol {
    pub fn id(&self) -> SymbolId {
        self.id
    }

    pub fn get_name(&self) -> &String {
        &self.info.name
    }

    pub fn set_name(&mut self, name: &str) {
        self.info.name = name.to_string();
    }

    pub fn get_kind(&self) -> SymbolKind {
        self.info.kind
    }

    pub fn set_kind(&mut self, kind: SymbolKind) {
        self.info.kind = kind;
    }

    pub fn get_type(&self) -> Option<TypeId> {
        self.info.ty
    }

    pub fn set_type(&mut self, ty: TypeId) {
        self.info.ty = Some(ty);
    }

    pub fn set_scope(&mut self, scope_id: scope::ScopeId) {
        self.info.scope = Some(scope_id)
    }

    pub fn get_scope(&self) -> Option<scope::ScopeId> {
        self.info.scope
    }

    pub fn get_defining_point(&self) -> Option<span::SpanLoc> {
        self.info.def_loc
    }

    pub fn set_defining_point(&mut self, span: span::SpanLoc) {
        self.info.def_loc = Some(span);
    }

    pub fn get_name_of_kind(&self) -> Option<&str> {
        match self.get_kind() {
            SymbolKind::Variable => Some("variable"),
            SymbolKind::Type => Some("type"),
            SymbolKind::Function => Some("function"),
            SymbolKind::Procedure => Some("procedure"),
            SymbolKind::Const => Some("const"),
            SymbolKind::Field => Some("field"),
            SymbolKind::BoundIdentifier => Some("bound identifier"),
            _ => None,
        }
    }

    pub fn get_const(&self) -> Option<Constant> {
        self.info.val.clone()
    }

    pub fn set_const(&mut self, val: Constant) {
        self.info.val = Some(val);
    }

    pub fn is_defined(&self) -> bool {
        self.info.defined
    }

    pub fn set_defined(&mut self, defined: bool) {
        self.info.defined = defined
    }

    pub fn get_parameter(&self) -> Option<ParameterKind> {
        self.info.parameter
    }

    pub fn set_parameter(&mut self, kind: ParameterKind) {
        self.info.parameter = Some(kind);
    }

    pub fn set_formal_parameters(&mut self, formal_parameters: Vec<Vec<SymbolId>>) {
        self.info.formal_parameters = Some(formal_parameters);
    }

    pub fn get_formal_parameters(&self) -> Option<Vec<Vec<SymbolId>>> {
        self.info.formal_parameters.clone()
    }

    pub fn set_return_symbol(&mut self, symbol_id: SymbolId) {
        self.info.return_symbol = Some(symbol_id);
    }

    pub fn get_return_symbol(&self) -> Option<SymbolId> {
        self.info.return_symbol
    }

    pub fn set_required(&mut self, required: bool) {
        self.info.required = required
    }

    pub fn is_required(&self) -> bool {
        self.info.required
    }

    pub fn add_to_required_environment(&mut self, symbol_id: SymbolId) {
        self.info.required_environment.insert(symbol_id);
    }

    pub fn get_required_environment(&self) -> &HashSet<SymbolId> {
        &self.info.required_environment
    }

    pub fn is_captured(&self) -> bool {
        self.info.captured
    }

    pub fn set_captured(&mut self, captured: bool) {
        self.info.captured = captured;
    }

    pub fn associated_record_type(&self) -> Option<TypeId> {
        self.info.associated_record_type
    }

    pub fn set_associated_record_type(&mut self, ty: TypeId) {
        self.info.associated_record_type = Some(ty);
    }

    pub fn associated_record(&self) -> Option<span::SpanId> {
        self.info.associated_record
    }

    pub fn set_associated_record(&mut self, id: span::SpanId) {
        self.info.associated_record = Some(id);
    }

    pub fn associated_field(&self) -> Option<SymbolId> {
        self.info.associated_field
    }

    pub fn set_associated_field(&mut self, id: SymbolId) {
        self.info.associated_field = Some(id);
    }
}

#[derive(Default)]
pub struct SymbolMap {
    symbols: Vec<Symbol>,
}

impl SymbolMap {
    pub fn new() -> SymbolMap {
        SymbolMap::default()
    }

    pub fn new_symbol(&mut self, mut sym: Symbol) -> SymbolId {
        assert!(sym.id().get_id() == usize::MAX, "Invalid symbol id");

        let new_id = SymbolId(self.symbols.len());
        sym.id = new_id;

        self.symbols.push(sym);

        new_id
    }

    pub fn get_symbol(&self, id: SymbolId) -> &Symbol {
        &self.symbols[id.get_id()]
    }

    pub fn get_symbol_mut(&mut self, id: SymbolId) -> &mut Symbol {
        &mut self.symbols[id.get_id()]
    }
}

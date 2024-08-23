use crate::constant::Constant;
use crate::scope;
use crate::span;
use crate::typesystem::TypeId;
use crate::utils;
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy, Ord, PartialOrd)]
pub struct SymbolId(utils::Identifier);

impl From<SymbolId> for utils::Identifier {
    fn from(id: SymbolId) -> utils::Identifier {
        id.0
    }
}

impl Default for SymbolId {
    fn default() -> SymbolId {
        SymbolId(utils::new_id())
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParameterKind {
    Value,
    Variable,
    Procedure,
    Function,
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
    // FIXME: We can reduce the memory used by this by grouping these less
    // common things by the kind of Symbol.
    parameter: Option<ParameterKind>,
    formal_parameters: Option<Vec<SymbolId>>,
    return_symbol: Option<SymbolId>,
    required_environment: HashSet<SymbolId>,
}

#[derive(Debug)]
pub struct Symbol {
    id: SymbolId,
    external_id: Cell<usize>,
    info: SymbolInfo,
}

impl Symbol {
    pub fn new() -> Symbol {
        let mut sym = Symbol {
            id: SymbolId::default(),
            external_id: Cell::new(usize::MAX),
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
        self.info.ty.clone()
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
        self.info.def_loc.clone()
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
            _ => None,
        }
    }

    pub fn has_external_id(&self) -> bool {
        return self.get_external_id() != usize::MAX;
    }

    pub fn get_external_id(&self) -> usize {
        self.external_id.get()
    }

    pub fn set_external_id(&self, external_id: usize) {
        self.external_id.set(external_id);
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

    pub fn set_formal_parameters(&mut self, formal_parameters: Vec<SymbolId>) {
        self.info.formal_parameters = Some(formal_parameters);
    }

    pub fn get_formal_parameters(&self) -> Option<Vec<SymbolId>> {
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
}

pub type SymbolMap = Rc<RefCell<SymbolMapImpl>>;
pub type SymbolRef = Rc<RefCell<Symbol>>;

#[derive(Default)]
pub struct SymbolMapImpl {
    symbols: HashMap<SymbolId, SymbolRef>,
}

impl SymbolMapImpl {
    pub fn new() -> SymbolMap {
        let x = SymbolMapImpl::default();

        Rc::new(RefCell::new(x))
    }

    pub fn new_symbol(&mut self, sym: Symbol) -> SymbolId {
        if let Some(sym) = self.symbols.get(&sym.id()) {
            return sym.borrow().id();
        }
        let new_id = sym.id();
        self.symbols.insert(new_id, Rc::new(RefCell::new(sym)));
        new_id
    }

    pub fn get_symbol(&self, id: SymbolId) -> SymbolRef {
        self.symbols.get(&id).unwrap().clone()
    }

    pub fn get_symbol_mut(&mut self, id: SymbolId) -> SymbolRef {
        self.symbols.get_mut(&id).unwrap().clone()
    }
}

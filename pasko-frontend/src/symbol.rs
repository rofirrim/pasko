use crate::constant::Constant;
use crate::span;
use crate::typesystem::TypeId;
use crate::utils;
use std::cell::Cell;

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
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
    Variable,
    Type,
    Function,
    Procedure,
    Const,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParameterKind {
    Value,
    Variable,
}

#[derive(Debug, Default)]
struct SymbolInfo {
    name: String,
    kind: SymbolKind,
    ty: Option<TypeId>,
    def_loc: Option<span::SpanLoc>,
    val: Option<Constant>,
    defined: bool,
    parameter: Option<ParameterKind>,
    formal_parameters: Option<Vec<SymbolId>>,
    return_symbol: Option<SymbolId>,
}

#[derive(Debug)]
pub struct Symbol {
    id: SymbolId,
    external_id: Cell<usize>,
    // Part that gets hashed.
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
}

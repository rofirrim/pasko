use crate::symbol::SymbolId;
use std::collections::HashMap;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct ScopeId(usize);

#[derive(Default)]
pub struct ScopeItem {
    symbol: Option<SymbolId>,
    parent: Option<ScopeId>,
    map: HashMap<String, SymbolId>,
}

pub struct Scope {
    current_scope: ScopeId,
    items: Vec<ScopeItem>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            current_scope: ScopeId(0),
            items: vec![ScopeItem::default()],
        }
    }

    pub fn get_current_scope_id(&self) -> ScopeId {
        self.current_scope
    }

    pub fn set_current_scope_id(&mut self, scope_id: ScopeId) {
        assert!(scope_id.0 < self.items.len());
        self.current_scope = scope_id
    }

    pub fn push_scope(&mut self, symbol: Option<SymbolId>) {
        let scope_item = ScopeItem {
            symbol,
            parent: Some(self.current_scope),
            ..Default::default()
        };
        self.items.push(scope_item);
        self.current_scope = ScopeId(self.items.len() - 1);
    }

    fn get_parent_scope_id(&self, scope_id: ScopeId) -> ScopeId {
        assert!(scope_id.0 > 0);
        assert!(self.items[scope_id.0].parent.is_some());
        self.items[scope_id.0].parent.unwrap()
    }

    pub fn pop_scope(&mut self) {
        self.current_scope = self.get_parent_scope_id(self.get_current_scope_id());
    }

    fn lookup_impl(&self, mut current_scope: ScopeId, name: &str) -> Option<SymbolId> {
        loop {
            if let Some(query) = self.items[current_scope.0].map.get(name) {
                return Some(*query);
            }
            if current_scope.0 == 0 {
                break;
            }
            current_scope = self.get_parent_scope_id(current_scope);
        }
        None
    }

    pub fn lookup(&self, name: &str) -> Option<SymbolId> {
        self.lookup_impl(self.current_scope, name)
    }

    pub fn lookup_current_scope(&self, name: &str) -> Option<SymbolId> {
        let current_scope = &self.items[self.current_scope.0];
        current_scope.map.get(name).cloned()
    }

    // Note: global scope includes the bottom scope (where program parameters live)
    // and the scope of the program block.
    pub fn lookup_global_scope(&self, name: &str) -> Option<SymbolId> {
        self.lookup_impl(ScopeId(1), name).map(|x| x)
    }

    pub fn get_global_scope_id(&self) -> ScopeId {
        ScopeId(1)
    }

    // Note: global scope includes only the bottom scope (where program parameters live)
    pub fn lookup_program_scope(&self, name: &str) -> Option<SymbolId> {
        self.lookup_impl(ScopeId(0), name).map(|x| x)
    }

    pub fn get_program_scope_id(&self) -> ScopeId {
        ScopeId(0)
    }

    pub fn add_entry_to_scope(&mut self, scope_id: ScopeId, name: &str, symbol: SymbolId) {
        assert!(scope_id.0 < self.items.len());
        self.items[scope_id.0].map.insert(name.to_string(), symbol);
    }

    pub fn add_entry(&mut self, name: &str, symbol: SymbolId) {
        self.add_entry_to_scope(self.current_scope, name, symbol)
    }

    // Global scope means the program parameters scope and the scope of the top
    // level block.
    pub fn add_entry_global_scope(&mut self, name: &str, symbol: SymbolId) {
        self.add_entry_to_scope(self.get_global_scope_id(), name, symbol)
    }

    // Program scope means only the program parameters scope.
    pub fn add_entry_program_scope(&mut self, name: &str, symbol: SymbolId) {
        self.add_entry_to_scope(self.get_program_scope_id(), name, symbol)
    }

    pub fn get_scope_symbol(&self) -> Option<SymbolId> {
        self.items[self.current_scope.0].symbol
    }

    pub fn get_scope_symbol_of_scope(&self, scope_id: ScopeId) -> Option<SymbolId> {
        self.items[scope_id.0].symbol
    }

    pub fn set_scope_symbol(&mut self, symbol: Option<SymbolId>) {
        self.items[self.current_scope.0].symbol = symbol
    }

    pub fn get_innermost_scope_symbol(&self, mut current_scope: ScopeId) -> Option<SymbolId> {
        loop {
            if let Some(scope_sym) = self.items[current_scope.0].symbol {
                return Some(scope_sym);
            }
            if current_scope.0 == 0 {
                break;
            }
            current_scope = self.get_parent_scope_id(current_scope);
        }
        None
    }

    pub fn scope_is_same_or_nested_in(&self, scope_id: ScopeId, possible_parent: ScopeId) -> bool {
        let mut current_scope = scope_id;
        loop {
            if current_scope == possible_parent {
                return true;
            }
            if current_scope.0 == 0 {
                break;
            }
            current_scope = self.get_parent_scope_id(current_scope);
        }
        false
    }

    pub fn scope_is_properly_nested_in(&self, scope_id: ScopeId, possible_parent: ScopeId) -> bool {
        scope_id != possible_parent && self.scope_is_same_or_nested_in(scope_id, possible_parent)
    }

    // Counts nesting distance crossing scopes with symbols.
    // FIXME: we could just traverse scopes up if we didn't create an additional scope for each block,
    // (though this would also remove program scope vs global scope).
    pub fn nesting_distance(&self, scope_id: ScopeId, parent_scope: ScopeId) -> usize {
        assert!(self.scope_is_same_or_nested_in(scope_id, parent_scope));
        let mut result = 0usize;
        let mut current_scope = scope_id;
        loop {
            if current_scope == parent_scope {
                return result;
            }
            if self.get_scope_symbol_of_scope(current_scope).is_some() {
                result += 1;
            }
            current_scope = self.get_parent_scope_id(current_scope);
        }
    }
}

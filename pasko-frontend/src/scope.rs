use crate::symbol::SymbolId;
use std::collections::HashMap;

#[derive(Default)]
pub struct ScopeItem {
    symbol: Option<SymbolId>,
    map: HashMap<String, SymbolId>,
}

pub struct Scope {
    stack: Vec<ScopeItem>,
}

const GLOBAL_SCOPE_IDX: usize = 1;

impl Scope {
    pub fn new() -> Scope {
        Scope {
            stack: vec![ScopeItem::default()],
        }
    }

    pub fn push_scope(&mut self, symbol: Option<SymbolId>) {
        self.stack.push(ScopeItem {
            symbol,
            map: HashMap::new(),
        });
    }

    pub fn is_global(&self) -> bool {
        // We always have a bottom scope, so 2.
        self.stack.len() == GLOBAL_SCOPE_IDX + 1
    }

    pub fn pop_scope(&mut self) {
        assert!(self.stack.len() > 1);
        self.stack.pop();
    }

    pub fn lookup(&self, name: &str) -> Option<SymbolId> {
        for scope in self.stack.iter().rev() {
            let query = scope.map.get(name);
            if query.is_some() {
                return query.cloned();
            }
        }
        None
    }

    pub fn lookup_current_scope(&self, name: &str) -> Option<SymbolId> {
        let current_scope = self.stack.last().unwrap();
        current_scope.map.get(name).cloned()
    }

    // Note: global scope includes the bottom scope (where program parameters live)
    // and the scope of the program block.
    pub fn lookup_global_scope(&self, name: &str) -> Option<SymbolId> {
        for scope in self.stack[0..=GLOBAL_SCOPE_IDX].iter().rev() {
            let query = scope.map.get(name);
            if query.is_some() {
                return query.cloned();
            }
        }
        None
    }

    // Note: global scope includes only the bottom scope (where program parameters live)
    pub fn lookup_program_scope(&self, name: &str) -> Option<SymbolId> {
        for scope in self.stack[0..=0].iter().rev() {
            let query = scope.map.get(name);
            if query.is_some() {
                return query.cloned();
            }
        }
        None
    }

    pub fn add_entry(&mut self, name: &str, symbol: SymbolId) {
        self.stack
            .last_mut()
            .unwrap()
            .map
            .insert(name.to_string(), symbol);
    }

    pub fn add_entry_global_scope(&mut self, name: &str, symbol: SymbolId) {
        self.stack[GLOBAL_SCOPE_IDX]
            .map
            .insert(name.to_string(), symbol);
    }

    pub fn add_entry_program_scope(&mut self, name: &str, symbol: SymbolId) {
        self.stack[GLOBAL_SCOPE_IDX]
            .map
            .insert(name.to_string(), symbol);
    }

    pub fn get_scope_symbol(&self) -> Option<SymbolId> {
        self.stack.last().unwrap().symbol
    }

    pub fn set_scope_symbol(&mut self, symbol: Option<SymbolId>) {
        self.stack.last_mut().unwrap().symbol = symbol;
    }

    pub fn get_innermost_scope_symbol(&self) -> Option<SymbolId> {
        for scope in self.stack.iter().rev() {
            let scope_sym = scope.symbol;
            if scope_sym.is_some() {
                return scope_sym;
            }
        }
        None
    }
}

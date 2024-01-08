use crate::symbol;
use crate::utils;
use std::hash::{Hash, Hasher};

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
pub struct TypeId(utils::Identifier);

impl From<TypeId> for utils::Identifier {
    fn from(id: TypeId) -> utils::Identifier {
        id.0
    }
}

impl Default for TypeId {
    fn default() -> TypeId {
        TypeId(utils::new_id())
    }
}
#[derive(Debug, Default, Clone, Eq, PartialEq, Hash)]
pub enum TypeKind {
    #[default]
    None,
    Integer,
    Real,
    Bool,
    Error,
    NamedType(symbol::SymbolId),
    String(usize),
}

#[derive(Debug, Default, Hash, PartialEq, Eq)]
struct TypeInfo {
    kind: TypeKind,
}

#[derive(Debug, Default)]
pub struct Type {
    id: TypeId,
    info: TypeInfo,
}

impl Type {
    pub fn id(&self) -> TypeId {
        self.id
    }

    pub fn get_kind(&self) -> TypeKind {
        self.info.kind.clone()
    }

    pub fn set_kind(&mut self, kind: TypeKind) {
        self.info.kind = kind;
    }

    pub fn is_error_type(&self) -> bool {
        match self.info.kind {
            TypeKind::Error => true,
            _ => false,
        }
    }

    pub fn is_integer_type(&self) -> bool {
        match self.info.kind {
            TypeKind::Integer => true,
            _ => false,
        }
    }

    pub fn is_real_type(&self) -> bool {
        match self.info.kind {
            TypeKind::Real => true,
            _ => false,
        }
    }

    pub fn is_bool_type(&self) -> bool {
        match self.info.kind {
            TypeKind::Bool => true,
            _ => false,
        }
    }

    pub fn get_symbol_of_named_type(&self) -> Option<symbol::SymbolId> {
        match self.info.kind {
            TypeKind::NamedType(sym_id) => Some(sym_id),
            _ => None,
        }
    }

    pub fn is_simple_type(&self) -> bool {
        match self.info.kind {
            TypeKind::Integer => true,
            TypeKind::Real => true,
            TypeKind::Bool => true,
            _ => false,
        }
    }

    pub fn is_ordinal_type(&self) -> bool {
        match self.info.kind {
            TypeKind::Integer => true,
            TypeKind::Bool => true,
            _ => false,
        }
    }

    pub fn is_string_type(&self) -> bool {
        match self.info.kind {
            TypeKind::String(_) => true,
            _ => false,
        }
    }

    pub fn string_type_len(&self) -> usize {
        match self.info.kind {
            TypeKind::String(len) => len,
            _ => {
                panic!("This is not a string type")
            }
        }
    }
}

impl Hash for Type {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.info.hash(state);
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.info.eq(&other.info)
    }
}

impl Eq for Type {}

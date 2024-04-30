use crate::constant;
use crate::constant::Constant;
use crate::limits;
use crate::symbol;
use crate::utils;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

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
    Char,
    Error,
    NamedType(symbol::SymbolId),
    Enum(Vec<symbol::SymbolId>),
    SubRange(TypeId, constant::Constant, constant::Constant),
    Array {
        packed: bool,
        index: TypeId,
        component: TypeId,
    },
    Record {
        packed: bool,
        fields: Vec<symbol::SymbolId>,
    },
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

    fn is_error_type(&self) -> bool {
        match self.info.kind {
            TypeKind::Error => true,
            _ => false,
        }
    }

    fn is_integer_type(&self) -> bool {
        match self.info.kind {
            TypeKind::Integer => true,
            _ => false,
        }
    }

    fn is_enum_type(&self) -> bool {
        match self.info.kind {
            TypeKind::Enum(..) => true,
            _ => false,
        }
    }

    fn enum_type_get_num_enumerators(&self) -> i64 {
        match &self.info.kind {
            TypeKind::Enum(v) => v.len() as i64,
            _ => panic!("Not an enum"),
        }
    }

    fn is_subrange_type(&self) -> bool {
        match self.info.kind {
            TypeKind::SubRange(..) => true,
            _ => false,
        }
    }

    fn subrange_type_get_lower(&self) -> &constant::Constant {
        match &self.info.kind {
            TypeKind::SubRange(_, lower, _) => lower,
            _ => panic!("Not a subrange"),
        }
    }

    fn subrange_type_get_upper(&self) -> &constant::Constant {
        match &self.info.kind {
            TypeKind::SubRange(_, _, upper) => upper,
            _ => panic!("Not a subrange"),
        }
    }

    fn get_host_type(&self) -> TypeId {
        match self.info.kind {
            TypeKind::SubRange(ty, ..) => ty,
            _ => panic!("This type has no host type"),
        }
    }

    fn is_array_type(&self) -> bool {
        match self.info.kind {
            TypeKind::Array { .. } => true,
            _ => false,
        }
    }

    fn array_type_get_index_type(&self) -> TypeId {
        match self.info.kind {
            TypeKind::Array { index, .. } => index,
            _ => panic!("This is not an array type"),
        }
    }

    fn array_type_get_component_type(&self) -> TypeId {
        match self.info.kind {
            TypeKind::Array { component, .. } => component,
            _ => panic!("This is not an array type"),
        }
    }

    fn array_type_is_packed(&self) -> bool {
        match self.info.kind {
            TypeKind::Array { packed, .. } => packed,
            _ => panic!("This is not an array type"),
        }
    }

    fn is_record_type(&self) -> bool {
        match self.info.kind {
            TypeKind::Record { .. } => true,
            _ => false,
        }
    }

    fn record_type_is_packed(&self) -> bool {
        match self.info.kind {
            TypeKind::Record { packed, .. } => packed,
            _ => panic!("This is not a record type"),
        }
    }

    fn record_type_get_fields(&self) -> &Vec<symbol::SymbolId> {
        match &self.info.kind {
            TypeKind::Record { fields, .. } => fields,
            _ => panic!("This is not a record type"),
        }
    }

    fn is_real_type(&self) -> bool {
        match self.info.kind {
            TypeKind::Real => true,
            _ => false,
        }
    }

    fn is_bool_type(&self) -> bool {
        match self.info.kind {
            TypeKind::Bool => true,
            _ => false,
        }
    }

    fn is_char_type(&self) -> bool {
        match self.info.kind {
            TypeKind::Char => true,
            _ => false,
        }
    }

    fn get_symbol_of_named_type(&self) -> Option<symbol::SymbolId> {
        match self.info.kind {
            TypeKind::NamedType(sym_id) => Some(sym_id),
            _ => None,
        }
    }

    fn is_simple_type(&self) -> bool {
        if self.is_ordinal_type() {
            return true;
        }
        match self.info.kind {
            TypeKind::Real => true,
            _ => false,
        }
    }

    fn is_ordinal_type(&self) -> bool {
        match self.info.kind {
            TypeKind::Integer
            | TypeKind::Bool
            | TypeKind::Char
            | TypeKind::Enum(..)
            | TypeKind::SubRange(..) => true,
            _ => false,
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

// TypeSystem

pub struct TypeSystem {
    types: HashMap<TypeId, Rc<Type>>,
    derived_types: HashSet<Rc<Type>>,

    integer_type_id: TypeId,
    real_type_id: TypeId,
    bool_type_id: TypeId,
    char_type_id: TypeId,
    error_type_id: TypeId,

    symbol_map: symbol::SymbolMap,
}

impl TypeSystem {
    pub fn new(symbol_map: symbol::SymbolMap) -> TypeSystem {
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

        let mut char_type = Type::default();
        char_type.set_kind(TypeKind::Char);
        let char_type_id = char_type.id();
        types.insert(char_type_id, Rc::new(char_type));

        let mut error_type = Type::default();
        error_type.set_kind(TypeKind::Error);
        let error_type_id = error_type.id();
        types.insert(error_type_id, Rc::new(error_type));

        Self {
            types,
            derived_types: HashSet::new(),
            integer_type_id,
            real_type_id,
            bool_type_id,
            char_type_id,
            error_type_id,
            symbol_map,
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

    pub fn get_integer_type(&self) -> TypeId {
        self.integer_type_id
    }

    pub fn is_integer_type(&self, ty: TypeId) -> bool {
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        ty.is_integer_type()
    }

    pub fn is_enum_type(&self, ty: TypeId) -> bool {
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        ty.is_enum_type()
    }

    pub fn is_subrange_type(&self, ty: TypeId) -> bool {
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        ty.is_subrange_type()
    }

    pub fn get_host_type(&self, ty: TypeId) -> TypeId {
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        ty.get_host_type()
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

    pub fn get_char_type(&self) -> TypeId {
        self.char_type_id
    }

    pub fn is_char_type(&self, ty: TypeId) -> bool {
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        ty.is_char_type()
    }

    pub fn get_error_type(&self) -> TypeId {
        self.error_type_id
    }

    pub fn is_error_type(&self, ty: TypeId) -> bool {
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        ty.is_error_type()
    }

    pub fn get_subrange_type_one_to_len(&mut self, len: usize) -> TypeId {
        let mut new_subrange_type = Type::default();
        new_subrange_type.set_kind(TypeKind::SubRange(
            self.get_integer_type(),
            Constant::Integer(1),
            Constant::Integer(len as i64),
        ));

        let new_subrange_type = Rc::new(new_subrange_type);
        match self.derived_types.get(&new_subrange_type.clone()) {
            Some(x) => return x.id(),
            _ => {}
        }

        let new_id = new_subrange_type.id();
        self.derived_types.insert(new_subrange_type.clone());
        self.types.insert(new_id, new_subrange_type);

        new_id
    }

    pub fn get_string_type(&mut self, len: usize) -> TypeId {
        // There are no strings of length 1!
        assert!(len > 1);
        let mut new_string_type = Type::default();

        new_string_type.set_kind(TypeKind::Array {
            packed: true,
            index: self.get_subrange_type_one_to_len(len),
            component: self.get_char_type(),
        });

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
        // FIXME: We could cache this.
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        if ty.is_array_type() {
            let index_ty = self.ultimate_type(ty.array_type_get_index_type());
            let index_ty = self.get_type_internal(index_ty);

            let component_ty = self.ultimate_type(ty.array_type_get_component_type());
            let component_ty = self.get_type_internal(component_ty);

            return ty.array_type_is_packed()
                && component_ty.is_char_type()
                && index_ty.is_subrange_type()
                && index_ty.subrange_type_get_lower() == &constant::Constant::Integer(1)
                && index_ty.subrange_type_get_upper() > &constant::Constant::Integer(1);
        }

        false
    }

    pub fn is_array_type(&self, ty: TypeId) -> bool {
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        ty.is_array_type()
    }

    pub fn array_type_get_component_type(&self, ty: TypeId) -> TypeId {
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        ty.array_type_get_component_type()
    }

    pub fn array_type_get_index_type(&self, ty: TypeId) -> TypeId {
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        ty.array_type_get_index_type()
    }

    pub fn is_record_type(&self, ty: TypeId) -> bool {
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        ty.is_record_type()
    }

    pub fn record_type_get_fields(&self, ty: TypeId) -> &Vec<symbol::SymbolId> {
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        ty.record_type_get_fields()
    }

    pub fn record_type_is_packed(&self, ty: TypeId) -> bool {
        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        ty.record_type_is_packed()
    }

    pub fn get_type_name(&self, id: TypeId) -> String {
        format!("{}", self.get_type_name_impl(id, false, HashSet::new()))
    }

    fn get_type_name_impl(
        &self,
        id: TypeId,
        skip_alias: bool,
        mut cycles: HashSet<TypeId>,
    ) -> String {
        if cycles.contains(&id) {
            return "...".to_string();
        }
        cycles.insert(id);
        let ty = self.get_type_internal(id);
        match ty.get_kind() {
            TypeKind::NamedType(sym_id) => {
                let sym = self.symbol_map.borrow().get_symbol(sym_id);
                let sym = sym.borrow();
                assert!(!self.is_builtin_type_name(sym.get_name()));
                if skip_alias {
                    return self.get_type_name_impl(self.ultimate_type(id), skip_alias, cycles);
                } else {
                    let aliased_to = self.get_type_name_impl(self.ultimate_type(id), true, cycles);
                    return format!("{} (an alias of {})", sym.get_name().clone(), aliased_to);
                }
            }
            TypeKind::Integer => "integer".to_string(),
            TypeKind::Real => "real".to_string(),
            TypeKind::Bool => "boolean".to_string(),
            TypeKind::SubRange(_host_type, lower, upper) => {
                format!("{}..{}", lower.to_string(), upper.to_string())
            }
            TypeKind::Char => "char".to_string(),
            TypeKind::Array {
                packed,
                index,
                component,
            } => {
                format!(
                    "{}array [{}] of {}",
                    if packed { "packed " } else { "" },
                    self.get_type_name_impl(index, skip_alias, cycles.clone()),
                    self.get_type_name_impl(component, skip_alias, cycles)
                )
            }
            TypeKind::Enum(enumerators) => {
                format!(
                    "({})",
                    enumerators
                        .iter()
                        .map(|sym_id| {
                            let sym = self.symbol_map.borrow().get_symbol(*sym_id);
                            let sym = sym.borrow();
                            sym.get_name().clone()
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            TypeKind::Record { packed, fields } => {
                format!(
                    "{}record {} end",
                    if packed { "packed" } else { "" },
                    fields
                        .iter()
                        .map(|sym_id| {
                            let sym = self.symbol_map.borrow().get_symbol(*sym_id);
                            let sym = sym.borrow();
                            format!(
                                "{} : {};",
                                sym.get_name(),
                                self.get_type_name_impl(
                                    sym.get_type().unwrap(),
                                    /* skip_alias */ true,
                                    cycles.clone()
                                )
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
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

    pub fn ultimate_type(&self, ty: TypeId) -> TypeId {
        let lhs_type = self.get_type_internal(ty);
        if let Some(sym_id) = lhs_type.get_symbol_of_named_type() {
            let sym = self.symbol_map.borrow().get_symbol(sym_id);
            let sym = sym.borrow();
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

    pub fn ordinal_type_lower_bound(&self, ty: TypeId) -> i64 {
        assert!(self.is_ordinal_type(ty));

        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        if ty.is_integer_type() {
            return -(limits::MAXINT as i64);
        } else if ty.is_bool_type() || ty.is_enum_type() || ty.is_char_type() {
            return 0;
        } else if ty.is_subrange_type() {
            if let Constant::Integer(lb) = ty.subrange_type_get_lower() {
                return *lb;
            }
        }

        panic!("Unhandled ordinal type {:?}", ty.get_kind());
    }

    pub fn ordinal_type_upper_bound(&self, ty: TypeId) -> i64 {
        assert!(self.is_ordinal_type(ty));

        let ty = self.ultimate_type(ty);
        let ty = self.get_type_internal(ty);

        if ty.is_integer_type() {
            return limits::MAXINT as i64;
        } else if ty.is_bool_type() {
            return 1;
        } else if ty.is_enum_type() {
            return ty.enum_type_get_num_enumerators();
        } else if ty.is_char_type() {
            return char::MAX as i64;
        } else if ty.is_subrange_type() {
            if let Constant::Integer(ub) = ty.subrange_type_get_upper() {
                return *ub;
            }
        }

        panic!("Unhandled ordinal type {:?}", ty.get_kind());
    }

    pub fn ordinal_type_extent(&self, ty: TypeId) -> i64 {
        let lower_bound = self.ordinal_type_lower_bound(ty);
        let upper_bound = self.ordinal_type_upper_bound(ty);

        if let Some(x) = upper_bound
            .checked_sub(lower_bound)
            .and_then(|x| x.checked_add(1))
        {
            return x;
        } else {
            panic!(
                "Overflow while computing the number of elements of ordinal type {}",
                self.get_type_name(ty)
            );
        }
    }
}

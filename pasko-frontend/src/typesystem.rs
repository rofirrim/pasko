use crate::constant;
use crate::constant::Constant;
use crate::limits;
use crate::symbol;
use crate::symbol::SymbolId;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
pub struct TypeId(pub usize);

impl From<TypeId> for usize {
    fn from(id: TypeId) -> usize {
        id.0
    }
}

impl Default for TypeId {
    fn default() -> TypeId {
        TypeId(usize::MAX)
    }
}

pub type FieldList = Vec<symbol::SymbolId>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct VariantCase {
    pub constants: Vec<constant::Constant>,
    pub fields: FieldList,
    pub variant: Option<VariantPart>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct VariantPart {
    pub tag_name: symbol::SymbolId,
    pub tag_type: TypeId,
    pub cases: Vec<VariantCase>,
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
    ConformableArray {
        packed: bool,
        lower: symbol::SymbolId,
        upper: symbol::SymbolId,
        component: TypeId,
    },
    Record {
        packed: bool,
        fixed_fields: FieldList,
        // Variants represented by their nesting level.
        variant: Option<VariantPart>,
        // For efficiency.
        all_fields: FieldList,
    },
    Set {
        packed: Option<bool>,
        element: TypeId,
    },
    GenericSet,
    Pointer(TypeId),
    GenericPointer,
    File {
        packed: bool,
        component: TypeId,
    },
    TextFile,
}

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
struct TypeInfo {
    kind: TypeKind,
}

#[derive(Debug, Default)]
pub struct Type {
    // The id is not part of the hash!
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
        matches!(self.info.kind, TypeKind::Error)
    }

    fn is_integer_type(&self) -> bool {
        matches!(self.info.kind, TypeKind::Integer)
    }

    fn is_enum_type(&self) -> bool {
        matches!(self.info.kind, TypeKind::Enum(..))
    }

    fn enum_type_get_num_enumerators(&self) -> i64 {
        match &self.info.kind {
            TypeKind::Enum(v) => v.len() as i64,
            _ => panic!("Not an enum"),
        }
    }

    fn enum_type_get_enumerators(&self) -> Vec<SymbolId> {
        match &self.info.kind {
            TypeKind::Enum(v) => v.clone(),
            _ => panic!("Not an enum"),
        }
    }

    fn is_subrange_type(&self) -> bool {
        matches!(self.info.kind, TypeKind::SubRange(..))
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
        matches!(self.info.kind, TypeKind::Array { .. })
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

    fn is_conformable_array_type(&self) -> bool {
        matches!(self.info.kind, TypeKind::ConformableArray { .. })
    }

    fn conformable_array_type_get_component_type(&self) -> TypeId {
        match self.info.kind {
            TypeKind::ConformableArray { component, .. } => component,
            _ => panic!("This is not an array type"),
        }
    }

    fn conformable_array_type_is_packed(&self) -> bool {
        match self.info.kind {
            TypeKind::ConformableArray { packed, .. } => packed,
            _ => panic!("This is not an array type"),
        }
    }

    fn conformable_array_type_get_lower(&self) -> SymbolId {
        match self.info.kind {
            TypeKind::ConformableArray { lower, .. } => lower,
            _ => panic!("This is not an array type"),
        }
    }

    fn conformable_array_type_get_upper(&self) -> SymbolId {
        match self.info.kind {
            TypeKind::ConformableArray { upper, .. } => upper,
            _ => panic!("This is not an array type"),
        }
    }

    fn is_record_type(&self) -> bool {
        matches!(self.info.kind, TypeKind::Record { .. })
    }

    fn record_type_is_packed(&self) -> bool {
        match self.info.kind {
            TypeKind::Record { packed, .. } => packed,
            _ => panic!("This is not a record type"),
        }
    }

    fn record_type_get_fixed_fields(&self) -> &Vec<symbol::SymbolId> {
        match &self.info.kind {
            TypeKind::Record { fixed_fields, .. } => fixed_fields,
            _ => panic!("This is not a record type"),
        }
    }

    fn record_type_get_all_fields(&self) -> &Vec<symbol::SymbolId> {
        match &self.info.kind {
            TypeKind::Record { all_fields, .. } => all_fields,
            _ => panic!("This is not a record type"),
        }
    }

    fn record_type_get_variant_part(&self) -> &Option<VariantPart> {
        match &self.info.kind {
            TypeKind::Record { variant, .. } => variant,
            _ => panic!("This is not a record type"),
        }
    }

    fn set_type_get_element_type(&self) -> TypeId {
        match self.info.kind {
            TypeKind::Set { element, .. } => element,
            _ => panic!("This is not a set type"),
        }
    }

    fn set_type_get_packed(&self) -> Option<bool> {
        match self.info.kind {
            TypeKind::Set { packed, .. } => packed,
            _ => panic!("This is not a set type"),
        }
    }

    fn is_set_type(&self) -> bool {
        matches!(self.info.kind, TypeKind::Set { .. })
    }

    fn is_generic_set_type(&self) -> bool {
        matches!(self.info.kind, TypeKind::GenericSet)
    }

    fn is_pointer_type(&self) -> bool {
        matches!(self.info.kind, TypeKind::Pointer(..))
    }

    fn is_generic_pointer_type(&self) -> bool {
        matches!(self.info.kind, TypeKind::GenericPointer)
    }

    fn pointer_type_get_pointee_type(&self) -> TypeId {
        match self.info.kind {
            TypeKind::Pointer(pointee) => pointee,
            _ => panic!("This is not a pointer type"),
        }
    }

    fn is_real_type(&self) -> bool {
        matches!(self.info.kind, TypeKind::Real)
    }

    fn is_bool_type(&self) -> bool {
        matches!(self.info.kind, TypeKind::Bool)
    }

    fn is_char_type(&self) -> bool {
        matches!(self.info.kind, TypeKind::Char)
    }

    fn get_symbol_of_named_type(&self) -> Option<symbol::SymbolId> {
        match self.info.kind {
            TypeKind::NamedType(sym_id) => Some(sym_id),
            _ => None,
        }
    }

    fn is_simple_type(&self) -> bool {
        self.is_ordinal_type() || matches!(self.info.kind, TypeKind::Real)
    }

    fn is_ordinal_type(&self) -> bool {
        matches!(
            self.info.kind,
            TypeKind::Integer
                | TypeKind::Bool
                | TypeKind::Char
                | TypeKind::Enum(..)
                | TypeKind::SubRange(..)
        )
    }

    fn is_file_type(&self) -> bool {
        matches!(self.info.kind, TypeKind::File { .. } | TypeKind::TextFile)
    }

    fn is_textfile_type(&self) -> bool {
        matches!(self.info.kind, TypeKind::TextFile)
    }

    fn file_type_get_component_type(&self) -> TypeId {
        match self.info.kind {
            TypeKind::File { component: ty, .. } => ty,
            _ => panic!("This is not a (non-text) file type"),
        }
    }

    // Does not clone the id!
    fn cloned_type(&self) -> Type {
        Type { id: TypeId::default(), info: self.info.clone() }
    }
}

impl Hash for Type {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        // id is not hashed
        self.info.hash(state);
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        // id is not hashed
        self.info.eq(&other.info)
    }
}

impl Eq for Type {}

// TypeSystem

pub struct TypeSystem {
    types: Vec<Type>,
    // Used to avoid creating (structurally) repeated types.
    derived_types: HashMap<Type, TypeId>,

    none_type_id: TypeId,
    integer_type_id: TypeId,
    real_type_id: TypeId,
    bool_type_id: TypeId,
    char_type_id: TypeId,
    error_type_id: TypeId,
    generic_set_type_id: TypeId,
    generic_pointer_type_id: TypeId,
    textfile_type_id: TypeId,
}

impl TypeSystem {
    pub fn new() -> TypeSystem {
        let mut types = Vec::new();

        let none_type = Type::default();
        let none_type_id = Self::insert_type(&mut types, none_type);

        let mut integer_type = Type::default();
        integer_type.set_kind(TypeKind::Integer);
        let integer_type_id = Self::insert_type(&mut types, integer_type);

        let mut real_type = Type::default();
        real_type.set_kind(TypeKind::Real);
        let real_type_id = Self::insert_type(&mut types, real_type);

        let mut bool_type = Type::default();
        bool_type.set_kind(TypeKind::Bool);
        let bool_type_id = Self::insert_type(&mut types, bool_type);

        let mut char_type = Type::default();
        char_type.set_kind(TypeKind::Char);
        let char_type_id = Self::insert_type(&mut types, char_type);

        let mut error_type = Type::default();
        error_type.set_kind(TypeKind::Error);
        let error_type_id = Self::insert_type(&mut types, error_type);

        let mut generic_set_type = Type::default();
        generic_set_type.set_kind(TypeKind::GenericSet);
        let generic_set_type_id = Self::insert_type(&mut types, generic_set_type);

        let mut generic_pointer_type = Type::default();
        generic_pointer_type.set_kind(TypeKind::GenericPointer);
        let generic_pointer_type_id = Self::insert_type(&mut types, generic_pointer_type);

        let mut textfile_type = Type::default();
        textfile_type.set_kind(TypeKind::TextFile);
        let textfile_type_id = Self::insert_type(&mut types, textfile_type);

        Self {
            types,
            derived_types: HashMap::new(),
            none_type_id,
            integer_type_id,
            real_type_id,
            bool_type_id,
            char_type_id,
            error_type_id,
            generic_set_type_id,
            generic_pointer_type_id,
            textfile_type_id,
        }
    }

    fn insert_type(types: &mut Vec<Type>, mut ty: Type) -> TypeId {
        assert!(ty.id.0 == usize::MAX, "Invalid identifier");

        let new_id = TypeId(types.len());

        ty.id = new_id;
        types.push(ty);

        new_id
    }

    pub fn new_type(&mut self, ty: Type) -> TypeId {
        Self::insert_type(&mut self.types, ty)
    }

    fn get_type_internal(&self, id: TypeId) -> &Type {
        &self.types[id.0]
    }

    pub fn get_none_type(&self) -> TypeId {
        self.none_type_id
    }

    pub fn get_integer_type(&self) -> TypeId {
        self.integer_type_id
    }

    pub fn is_integer_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.is_integer_type()
    }

    pub fn is_enum_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.is_enum_type()
    }

    pub fn enum_type_get_enumerators(
        &self,
        ty: TypeId,
        symbol_map: &symbol::SymbolMap,
    ) -> Vec<SymbolId> {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.enum_type_get_enumerators()
    }

    pub fn is_subrange_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.is_subrange_type()
    }

    pub fn get_host_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> TypeId {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.get_host_type()
    }

    pub fn get_real_type(&self) -> TypeId {
        self.real_type_id
    }

    pub fn is_real_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.is_real_type()
    }

    pub fn get_bool_type(&self) -> TypeId {
        self.bool_type_id
    }

    pub fn is_bool_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.is_bool_type()
    }

    pub fn get_char_type(&self) -> TypeId {
        self.char_type_id
    }

    pub fn is_char_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.is_char_type()
    }

    pub fn get_error_type(&self) -> TypeId {
        self.error_type_id
    }

    pub fn is_error_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.is_error_type()
    }

    fn insert_and_cache(&mut self, ty: Type) -> TypeId { 
        if let Some(x) = self.derived_types.get(&ty) {
            return *x;
        }

        let cached_ty = ty.cloned_type();
        let new_id = self.new_type(ty);
        self.derived_types.insert(cached_ty, new_id);
        new_id
    }

    pub fn get_subrange_type_one_to_len(&mut self, len: usize) -> TypeId {
        let mut new_subrange_type = Type::default();
        new_subrange_type.set_kind(TypeKind::SubRange(
            self.get_integer_type(),
            Constant::Integer(1),
            Constant::Integer(len as i64),
        ));

        self.insert_and_cache(new_subrange_type)
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

        self.insert_and_cache(new_string_type)
    }

    pub fn is_string_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        // FIXME: We could cache this.
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        if ty.is_array_type() {
            let index_ty = self.ultimate_type(ty.array_type_get_index_type(), symbol_map);
            let index_ty = self.get_type_internal(index_ty);

            let component_ty = self.ultimate_type(ty.array_type_get_component_type(), symbol_map);
            let component_ty = self.get_type_internal(component_ty);

            return ty.array_type_is_packed()
                && component_ty.is_char_type()
                && index_ty.is_subrange_type()
                && index_ty.subrange_type_get_lower() == &constant::Constant::Integer(1)
                && index_ty.subrange_type_get_upper() > &constant::Constant::Integer(1);
        }

        false
    }

    pub fn is_array_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.is_array_type()
    }

    pub fn array_type_is_packed(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.array_type_is_packed()
    }

    pub fn array_type_get_component_type(
        &self,
        ty: TypeId,
        symbol_map: &symbol::SymbolMap,
    ) -> TypeId {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.array_type_get_component_type()
    }

    pub fn array_type_get_index_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> TypeId {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.array_type_get_index_type()
    }

    pub fn is_conformable_array_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.is_conformable_array_type()
    }

    pub fn conformable_array_type_get_component_type(
        &self,
        ty: TypeId,
        symbol_map: &symbol::SymbolMap,
    ) -> TypeId {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.conformable_array_type_get_component_type()
    }

    pub fn conformable_array_type_is_packed(
        &self,
        ty: TypeId,
        symbol_map: &symbol::SymbolMap,
    ) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.conformable_array_type_is_packed()
    }

    pub fn conformable_array_type_get_lower(
        &self,
        ty: TypeId,
        symbol_map: &symbol::SymbolMap,
    ) -> SymbolId {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.conformable_array_type_get_lower()
    }

    pub fn conformable_array_type_get_upper(
        &self,
        ty: TypeId,
        symbol_map: &symbol::SymbolMap,
    ) -> SymbolId {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.conformable_array_type_get_upper()
    }

    pub fn array_or_conformable_array_type_get_component_type(
        &self,
        ty: TypeId,
        symbol_map: &symbol::SymbolMap,
    ) -> TypeId {
        if self.is_array_type(ty, symbol_map) {
            self.array_type_get_component_type(ty, symbol_map)
        } else {
            self.conformable_array_type_get_component_type(ty, symbol_map)
        }
    }

    pub fn array_or_conformable_array_type_get_index_type(
        &self,
        ty: TypeId,
        symbol_map: &symbol::SymbolMap,
    ) -> TypeId {
        if self.is_array_type(ty, symbol_map) {
            let ty = self.ultimate_type(ty, symbol_map);
            let ty = self.get_type_internal(ty);

            ty.array_type_get_index_type()
        } else {
            let lower = self.conformable_array_type_get_lower(ty, symbol_map);
            let lower_sym = symbol_map.get_symbol(lower);
            lower_sym.get_type().unwrap()
        }
    }

    pub fn is_record_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.is_record_type()
    }

    pub fn record_type_get_all_fields(
        &self,
        ty: TypeId,
        symbol_map: &symbol::SymbolMap,
    ) -> &Vec<symbol::SymbolId> {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.record_type_get_all_fields()
    }

    pub fn record_type_get_fixed_fields(
        &self,
        ty: TypeId,
        symbol_map: &symbol::SymbolMap,
    ) -> &Vec<symbol::SymbolId> {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.record_type_get_fixed_fields()
    }

    pub fn record_type_get_variant_part(
        &self,
        ty: TypeId,
        symbol_map: &symbol::SymbolMap,
    ) -> &Option<VariantPart> {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.record_type_get_variant_part()
    }

    pub fn record_type_is_packed(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.record_type_is_packed()
    }

    pub fn get_generic_set_type(&self) -> TypeId {
        self.generic_set_type_id
    }

    pub fn is_generic_set_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.is_generic_set_type()
    }

    pub fn get_set_type(&mut self, packed: Option<bool>, element: TypeId) -> TypeId {
        let mut new_set_type = Type::default();
        new_set_type.set_kind(TypeKind::Set { packed, element });

        self.insert_and_cache(new_set_type)
    }

    pub fn is_set_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.is_set_type()
    }

    pub fn set_type_get_packed(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> Option<bool> {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.set_type_get_packed()
    }

    pub fn set_type_get_element(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> TypeId {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.set_type_get_element_type()
    }

    pub fn get_generic_pointer_type(&self) -> TypeId {
        self.generic_pointer_type_id
    }

    pub fn is_generic_pointer_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.is_generic_pointer_type()
    }

    pub fn get_pointer_type(&mut self, ty: TypeId) -> TypeId {
        let mut new_pointer_type = Type::default();
        new_pointer_type.set_kind(TypeKind::Pointer(ty));

        self.insert_and_cache(new_pointer_type)
    }

    pub fn is_pointer_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.is_pointer_type()
    }

    pub fn pointer_type_get_pointee_type(
        &self,
        ty: TypeId,
        symbol_map: &symbol::SymbolMap,
    ) -> TypeId {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.pointer_type_get_pointee_type()
    }

    pub fn get_file_type(&mut self, packed: bool, component: TypeId) -> TypeId {
        let mut new_file_type = Type::default();
        new_file_type.set_kind(TypeKind::File { packed, component });

        self.insert_and_cache(new_file_type)
    }

    pub fn get_textfile_type(&self) -> TypeId {
        self.textfile_type_id
    }

    pub fn is_file_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.is_file_type()
    }

    pub fn is_textfile_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.is_textfile_type()
    }

    pub fn file_type_get_component_type(
        &self,
        ty: TypeId,
        symbol_map: &symbol::SymbolMap,
    ) -> TypeId {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        if ty.is_textfile_type() {
            self.get_char_type()
        } else {
            ty.file_type_get_component_type()
        }
    }

    pub fn is_named_type(&self, id: TypeId) -> bool {
        let ty = self.get_type_internal(id);
        match ty.get_kind() {
            TypeKind::NamedType(..) => true,
            _ => false,
        }
    }

    pub fn named_type_get_symbol(&self, id: TypeId) -> SymbolId {
        self.get_type_internal(id)
            .get_symbol_of_named_type()
            .unwrap()
    }

    pub fn get_type_name(&self, id: TypeId, symbol_map: &symbol::SymbolMap) -> String {
        self.get_type_name_impl(id, false, HashSet::new(), symbol_map)
            .to_string()
    }

    fn print_variant_part(
        &self,
        variant: &Option<VariantPart>,
        skip_alias: bool,
        cycles: HashSet<TypeId>,
        symbol_map: &symbol::SymbolMap,
    ) -> String {
        if let Some(variant_part) = variant {
            format!(
                "case {}{} of {}",
                {
                    let sym = symbol_map.get_symbol(variant_part.tag_name);
                    format!("{} : ", sym.get_name())
                },
                self.get_type_name_impl(
                    variant_part.tag_type,
                    skip_alias,
                    cycles.clone(),
                    symbol_map
                ),
                variant_part
                    .cases
                    .iter()
                    .map(|case| {
                        format!(
                            "{}{}",
                            {
                                format!(
                                    "{}: ({});",
                                    case.constants
                                        .iter()
                                        .map(|x| x.to_string())
                                        .collect::<Vec<_>>()
                                        .join(", "),
                                    case.fields
                                        .iter()
                                        .map(|field_sym| {
                                            let sym = symbol_map.get_symbol(*field_sym);
                                            format!(
                                                "{}: {};",
                                                sym.get_name(),
                                                self.get_type_name_impl(
                                                    sym.get_type().unwrap(),
                                                    /* skip_alias */ true,
                                                    cycles.clone(),
                                                    symbol_map
                                                )
                                            )
                                        })
                                        .collect::<Vec<_>>()
                                        .join(" "),
                                )
                            },
                            {
                                self.print_variant_part(
                                    &case.variant,
                                    skip_alias,
                                    cycles.clone(),
                                    symbol_map,
                                )
                            }
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(" ")
            )
        } else {
            "".to_string()
        }
    }

    fn get_type_name_impl(
        &self,
        id: TypeId,
        skip_alias: bool,
        mut cycles: HashSet<TypeId>,
        symbol_map: &symbol::SymbolMap,
    ) -> String {
        let ty = self.get_type_internal(id);
        let was_already_visited = cycles.contains(&id);
        cycles.insert(id);
        match ty.get_kind() {
            TypeKind::NamedType(sym_id) => {
                let sym = symbol_map.get_symbol(sym_id);
                assert!(!self.is_builtin_type_name(sym.get_name()));
                if was_already_visited {
                    return sym.get_name().clone();
                } else if skip_alias {
                    return self.get_type_name_impl(
                        self.ultimate_type(id, symbol_map),
                        skip_alias,
                        cycles,
                        symbol_map,
                    );
                } else {
                    let aliased_to = self.get_type_name_impl(
                        self.ultimate_type(id, symbol_map),
                        true,
                        cycles,
                        symbol_map,
                    );
                    return format!("{} (an alias of {})", sym.get_name().clone(), aliased_to);
                }
            }
            TypeKind::Integer => "integer".to_string(),
            TypeKind::Real => "real".to_string(),
            TypeKind::Bool => "boolean".to_string(),
            TypeKind::SubRange(_host_type, lower, upper) => {
                format!("{}..{}", lower, upper)
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
                    self.get_type_name_impl(index, skip_alias, cycles.clone(), symbol_map),
                    self.get_type_name_impl(component, skip_alias, cycles, symbol_map)
                )
            }
            TypeKind::ConformableArray {
                packed,
                lower,
                upper,
                component,
            } => {
                format!(
                    "conformable {}array [{}..{}] of {}",
                    if packed { "packed " } else { "" },
                    {
                        let sym = symbol_map.get_symbol(lower);
                        sym.get_name().clone()
                    },
                    {
                        let sym = symbol_map.get_symbol(upper);
                        sym.get_name().clone()
                    },
                    self.get_type_name_impl(component, skip_alias, cycles, symbol_map)
                )
            }
            TypeKind::Enum(enumerators) => {
                format!(
                    "({})",
                    enumerators
                        .iter()
                        .map(|sym_id| {
                            let sym = symbol_map.get_symbol(*sym_id);
                            sym.get_name().clone()
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            TypeKind::Record {
                packed,
                fixed_fields,
                variant,
                ..
            } => {
                format!(
                    "{}record {}{} end",
                    if packed { "packed " } else { "" },
                    // FIXME, we should not be flattening the fields.
                    fixed_fields
                        .iter()
                        .map(|sym_id| {
                            let sym = symbol_map.get_symbol(*sym_id);
                            format!(
                                "{} : {};",
                                sym.get_name(),
                                self.get_type_name_impl(
                                    sym.get_type().unwrap(),
                                    /* skip_alias */ true,
                                    cycles.clone(),
                                    symbol_map,
                                )
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(" "),
                    self.print_variant_part(&variant, skip_alias, cycles, symbol_map)
                )
            }
            TypeKind::Set { packed, element } => {
                format!(
                    "{}set of {}",
                    if packed.is_some() {
                        if packed.unwrap() {
                            "packed "
                        } else {
                            ""
                        }
                    } else {
                        "<no packedness> "
                    },
                    self.get_type_name_impl(element, skip_alias, cycles.clone(), symbol_map)
                )
            }
            TypeKind::GenericSet => "any set".to_owned(),
            TypeKind::GenericPointer => "generic pointer".to_owned(),
            TypeKind::Pointer(pointee) => format!(
                "^{}",
                self.get_type_name_impl(pointee, skip_alias, cycles.clone(), symbol_map),
            ),
            TypeKind::File { packed, component } => format!(
                "{}file of {}",
                if packed { "packed " } else { "" },
                self.get_type_name_impl(component, skip_alias, cycles.clone(), symbol_map)
            ),
            TypeKind::TextFile => "text".to_owned(),
            TypeKind::None => "<no type>".to_owned(),
            TypeKind::Error => "<error-type>".to_owned(),
        }
    }

    pub fn is_builtin_type_name(&self, name: &str) -> bool {
        matches!(name, "integer" | "real" | "boolean" | "char" | "text")
    }

    pub fn is_none_type(&self, a: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let a = self.ultimate_type(a, symbol_map);

        a == self.none_type_id
    }

    pub fn ultimate_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> TypeId {
        let lhs_type = self.get_type_internal(ty);
        if let Some(sym_id) = lhs_type.get_symbol_of_named_type() {
            let sym = symbol_map.get_symbol(sym_id);
            return self.ultimate_type(sym.get_type().unwrap(), symbol_map);
        }
        ty
    }

    pub fn same_type(&self, a: TypeId, b: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let id_a = self.get_type_internal(a).get_kind();
        let id_b = self.get_type_internal(b).get_kind();
        match (id_a, id_b) {
            // None is never the same as other types
            (TypeKind::None, _) => false,
            (_, TypeKind::None) => false,
            // See through named types
            (TypeKind::NamedType(_), _) => {
                self.same_type(self.ultimate_type(a, symbol_map), b, symbol_map)
            }
            (_, TypeKind::NamedType(_)) => {
                self.same_type(a, self.ultimate_type(b, symbol_map), symbol_map)
            }
            // Structural checking
            (TypeKind::SubRange(t1, ca1, cb1), TypeKind::SubRange(t2, ca2, cb2)) => {
                ca1 == ca2 && cb1 == cb2 && self.same_type(t1, t2, symbol_map)
            }
            (TypeKind::Pointer(p1), TypeKind::Pointer(p2)) => self.same_type(p1, p2, symbol_map),
            (
                TypeKind::Set {
                    packed: p1,
                    element: s1,
                },
                TypeKind::Set {
                    packed: p2,
                    element: s2,
                },
            ) => (p1.is_some() == p2.is_some()) && self.same_type(s1, s2, symbol_map),
            (
                TypeKind::Array {
                    packed: p1,
                    index: i1,
                    component: c1,
                },
                TypeKind::Array {
                    packed: p2,
                    index: i2,
                    component: c2,
                },
            ) => {
                (p1 == p2)
                    && self.same_type(i1, i2, symbol_map)
                    && self.same_type(c1, c2, symbol_map)
            }
            (
                TypeKind::File {
                    packed: p1,
                    component: c1,
                },
                TypeKind::File {
                    packed: p2,
                    component: c2,
                },
            ) => p1 == p2 && self.same_type(c1, c2, symbol_map),
            // Integer, Real, Bool, Char, Error, Enum and Record
            // GenericSet, GenericPointer
            _ => a == b,
        }
    }

    pub fn is_valid_component_type_of_file_type(
        &self,
        ty: TypeId,
        symbol_map: &symbol::SymbolMap,
    ) -> bool {
        if self.is_file_type(ty, symbol_map) {
            return false;
        } else if self.is_record_type(ty, symbol_map) {
            let fields = self.record_type_get_all_fields(ty, symbol_map);

            fields.iter().all(|field| {
                let sym = symbol_map.get_symbol(*field);

                self.is_valid_component_type_of_file_type(sym.get_type().unwrap(), symbol_map)
            });
        }

        true
    }

    pub fn is_simple_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.is_simple_type()
    }

    pub fn is_ordinal_type(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> bool {
        let ty = self.ultimate_type(ty, symbol_map);
        let ty = self.get_type_internal(ty);

        ty.is_ordinal_type()
    }

    pub fn ordinal_type_lower_bound(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> i64 {
        assert!(self.is_ordinal_type(ty, symbol_map));

        let ty = self.ultimate_type(ty, symbol_map);
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

    pub fn ordinal_type_upper_bound(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> i64 {
        assert!(self.is_ordinal_type(ty, symbol_map));

        let ty = self.ultimate_type(ty, symbol_map);
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

    pub fn ordinal_type_extent(&self, ty: TypeId, symbol_map: &symbol::SymbolMap) -> i64 {
        let lower_bound = self.ordinal_type_lower_bound(ty, symbol_map);
        let upper_bound = self.ordinal_type_upper_bound(ty, symbol_map);

        if let Some(x) = upper_bound
            .checked_sub(lower_bound)
            .and_then(|x| x.checked_add(1))
        {
            x
        } else {
            panic!(
                "Overflow while computing the number of elements of ordinal type {}",
                self.get_type_name(ty, symbol_map)
            );
        }
    }
}

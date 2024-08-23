#![allow(unused_imports)]

use cranelift_codegen::ir::function::FunctionParameters;
use cranelift_codegen::settings::Configurable;
use cranelift_codegen::write::PlainWriter;
use pasko_frontend::ast::{self, FormalParameter};
use pasko_frontend::semantic::SemanticContext;
use pasko_frontend::span;
use pasko_frontend::symbol::{ParameterKind, SymbolId};
use pasko_frontend::typesystem::{TypeId, VariantPart};
use pasko_frontend::visitor::{Visitable, VisitorMut};

use cranelift_codegen::ir::types::{F64, I32, I64, I8};
use cranelift_codegen::ir::{
    function, AbiParam, FuncRef, Function, Signature, StackSlotData, StackSlotKind, UserFuncName,
};
use cranelift_codegen::isa::{CallConv, TargetFrontendConfig};
use cranelift_codegen::settings;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{DataDescription, DataId, Module};

use cranelift_codegen::verifier::verify_function;

use cranelift_codegen::ir::InstBuilder;
use cranelift_module::Linkage;
use cranelift_object;

use std::any::Any;
use std::collections::HashMap;
use std::fs::File;
use std::hash::Hash;
use std::io::BufWriter;
use std::io::Write;

use std::cell::RefCell;

use crate::datalocation::DataLocation;
use crate::function::FunctionCodegenVisitor;
use crate::runtime::RuntimeFunctionId;

#[derive(Debug, Clone)]
struct SizeAndAlignment {
    size: usize,
    align: usize,
}

pub struct CodegenVisitor<'a> {
    pub object_module: Option<Box<cranelift_object::ObjectModule>>,
    pub ctx: cranelift_codegen::Context,
    pub semantic_context: &'a SemanticContext,
    pub pointer_type: cranelift_codegen::ir::Type,
    pub string_table: HashMap<String, cranelift_module::DataId>,

    pub data_location: HashMap<SymbolId, DataLocation>,
    pub function_identifiers: HashMap<SymbolId, cranelift_module::FuncId>,
    pub function_names: HashMap<cranelift_module::FuncId, String>,
    pub global_names: HashMap<cranelift_module::DataId, String>,
    pub offset_cache: RefCell<HashMap<SymbolId, usize>>,
    pub annotations: EntityAnnotations,

    ir_dump: bool,
    globals_to_dispose: Vec<SymbolId>,
    size_align_cache: RefCell<HashMap<TypeId, SizeAndAlignment>>,
    trivially_copiable_cache: RefCell<HashMap<TypeId, bool>>,

    input_data_id: Option<cranelift_module::DataId>,
    output_data_id: Option<cranelift_module::DataId>,

    rt_functions_cache: HashMap<RuntimeFunctionId, cranelift_module::FuncId>,
}

#[derive(Default)]
pub struct EntityAnnotations {
    annotations: HashMap<cranelift_codegen::ir::entities::AnyEntity, String>,
}

impl EntityAnnotations {
    pub fn get(&self, entity: cranelift_codegen::ir::entities::AnyEntity) -> Option<String> {
        self.annotations.get(&entity).cloned()
    }

    fn new(&mut self, entity: cranelift_codegen::ir::entities::AnyEntity, text: &str) {
        self.annotations.insert(entity, text.to_string());
    }

    fn clear_temporary(&mut self) {
        let stack_slots: HashMap<_, _> = self
            .annotations
            .iter()
            .filter(|(&e, _)| match e {
                cranelift_codegen::ir::entities::AnyEntity::StackSlot { .. } => false,
                _ => true,
            })
            .map(|(a, b)| (*a, b.clone()))
            .collect();

        self.annotations = stack_slots;
    }

    pub fn new_function_ref(&mut self, func_ref: cranelift_codegen::ir::FuncRef, text: &str) {
        self.new(
            cranelift_codegen::ir::entities::AnyEntity::FuncRef(func_ref),
            text,
        );
    }

    pub fn new_stack_slot(&mut self, stack_slot: cranelift_codegen::ir::StackSlot, text: &str) {
        self.new(
            cranelift_codegen::ir::entities::AnyEntity::StackSlot(stack_slot),
            text,
        );
    }

    pub fn new_global_value(
        &mut self,
        global_value: cranelift_codegen::ir::GlobalValue,
        text: &str,
    ) {
        self.new(
            cranelift_codegen::ir::entities::AnyEntity::GlobalValue(global_value),
            text,
        );
    }
}

struct AnnotatedFuncWriter<'a> {
    annotations: &'a EntityAnnotations,
}

impl<'a> AnnotatedFuncWriter<'a> {
    fn new(annotations: &'a EntityAnnotations) -> Self {
        Self { annotations }
    }
}

impl<'a> cranelift_codegen::write::FuncWriter for AnnotatedFuncWriter<'a> {
    fn write_block_header(
        &mut self,
        w: &mut dyn std::fmt::Write,
        func: &cranelift_codegen::ir::Function,
        block: cranelift_codegen::ir::Block,
        indent: usize,
    ) -> std::fmt::Result {
        let mut p = PlainWriter;
        p.write_block_header(w, func, block, indent)
    }
    fn write_instruction(
        &mut self,
        w: &mut dyn std::fmt::Write,
        func: &Function,
        aliases: &cranelift_entity::SecondaryMap<
            cranelift_codegen::ir::Value,
            Vec<cranelift_codegen::ir::Value>,
        >,
        inst: cranelift_codegen::ir::Inst,
        indent: usize,
    ) -> std::fmt::Result {
        let mut p = PlainWriter;
        p.write_instruction(w, func, aliases, inst, indent)
    }

    fn write_entity_definition(
        &mut self,
        w: &mut dyn std::fmt::Write,
        _func: &cranelift_codegen::ir::Function,
        entity: cranelift_codegen::ir::entities::AnyEntity,
        value: &dyn std::fmt::Display,
        maybe_fact: Option<&cranelift_codegen::ir::pcc::Fact>,
    ) -> std::fmt::Result {
        if let Some(fact) = maybe_fact {
            write!(w, "    {} ! {} = {}", entity, fact, value)?;
        } else {
            write!(w, "    {} = {}", entity, value)?;
        }

        if let Some(annot) = self.annotations.get(entity) {
            write!(w, " ; {}", annot)?;
        }

        writeln!(w)
    }
}

impl<'a> CodegenVisitor<'a> {
    pub fn new(semantic_context: &'a SemanticContext, ir_dump: bool) -> CodegenVisitor<'a> {
        let mut flag_builder = settings::builder();
        let isa_builder =
            cranelift_codegen::isa::lookup_by_name("x86_64-unknown-linux-gnu").unwrap();
        flag_builder.set("is_pic", "true").unwrap();
        flag_builder.set("opt_level", "speed").unwrap();
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let pointer_type = isa.pointer_type();
        let object_module = cranelift_object::ObjectModule::new(
            cranelift_object::ObjectBuilder::new(
                isa,
                "pasko-program",
                cranelift_module::default_libcall_names(),
            )
            .unwrap(),
        );
        let ctx = cranelift_codegen::Context::new();

        // Initialize environment

        let visitor = CodegenVisitor {
            object_module: Some(Box::new(object_module)),
            ctx,
            semantic_context,
            pointer_type,
            string_table: HashMap::new(),
            data_location: HashMap::new(),
            function_identifiers: HashMap::new(),
            function_names: HashMap::new(),
            global_names: HashMap::new(),
            offset_cache: RefCell::new(HashMap::new()),
            annotations: EntityAnnotations::default(),
            // Private
            ir_dump,
            globals_to_dispose: vec![],
            size_align_cache: RefCell::new(HashMap::new()),
            trivially_copiable_cache: RefCell::new(HashMap::new()),
            input_data_id: None,
            output_data_id: None,
            rt_functions_cache: HashMap::new(),
        };

        visitor
    }

    pub fn emit_object(&mut self, obj_filename: &str) {
        let object_product = self.object_module.take().unwrap().finish();

        let result = object_product.emit().unwrap();

        let mut file = File::create(obj_filename).unwrap();
        file.write_all(&result).unwrap();
    }

    fn register_import(&mut self, name: &str, sig: Signature) -> Option<cranelift_module::FuncId> {
        let func_id = self
            .object_module
            .as_mut()
            .unwrap()
            .declare_function(name, Linkage::Import, &sig)
            .unwrap();
        self.function_names.insert(func_id, name.to_string());
        Some(func_id)
    }

    pub fn get_runtime_function(&mut self, rtid: RuntimeFunctionId) -> cranelift_module::FuncId {
        if let Some(func_id) = self.rt_functions_cache.get(&rtid) {
            return *func_id;
        }
        // FIXME: Convert this into a table instead.
        let import_id: Option<cranelift_module::FuncId> = match rtid {
            RuntimeFunctionId::WriteTextfileStr => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // file
                sig.params.push(AbiParam::new(self.pointer_type)); // string
                sig.params.push(AbiParam::new(I64)); // number of chars
                self.register_import("__pasko_write_textfile_str", sig)
            }
            RuntimeFunctionId::WriteTextfileI64 => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // file
                sig.params.push(AbiParam::new(I64)); // number
                sig.params.push(AbiParam::new(I64)); // total_width
                self.register_import("__pasko_write_textfile_i64", sig)
            }
            RuntimeFunctionId::WriteTextfileF64 => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // file
                sig.params.push(AbiParam::new(F64)); // number
                sig.params.push(AbiParam::new(I64)); // total_width
                sig.params.push(AbiParam::new(I64)); // frac_digits
                self.register_import("__pasko_write_textfile_f64", sig)
            }
            RuntimeFunctionId::WriteTextfileBool => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // file
                sig.params.push(AbiParam::new(I8)); // boolean
                self.register_import("__pasko_write_textfile_bool", sig)
            }
            RuntimeFunctionId::WriteTextfileChar => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // file
                sig.params.push(AbiParam::new(I32)); // number
                self.register_import("__pasko_write_textfile_char", sig)
            }
            RuntimeFunctionId::WriteTextfileNewline => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // file
                self.register_import("__pasko_write_textfile_newline", sig)
            }
            RuntimeFunctionId::ReadTextfileI64 => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // file
                sig.returns.push(AbiParam::new(I64));
                self.register_import("__pasko_read_textfile_i64", sig)
            }
            RuntimeFunctionId::ReadTextfileF64 => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // file
                sig.returns.push(AbiParam::new(F64));
                self.register_import("__pasko_read_textfile_f64", sig)
            }
            RuntimeFunctionId::ReadTextfileNewline => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // file
                self.register_import("__pasko_read_textfile_newline", sig)
            }
            RuntimeFunctionId::SetNew => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(I64)); // N
                sig.params.push(AbiParam::new(self.pointer_type)); // values
                sig.returns.push(AbiParam::new(self.pointer_type)); // address to new set
                self.register_import("__pasko_set_new", sig)
            }
            RuntimeFunctionId::SetDispose => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // address to set
                self.register_import("__pasko_set_dispose", sig)
            }
            RuntimeFunctionId::SetCopy => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // address to src set
                sig.returns.push(AbiParam::new(self.pointer_type)); // address to new set
                self.register_import("__pasko_set_copy", sig)
            }
            RuntimeFunctionId::SetUnion => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // x
                sig.params.push(AbiParam::new(self.pointer_type)); // y
                sig.returns.push(AbiParam::new(self.pointer_type)); // result
                self.register_import("__pasko_set_union", sig)
            }
            RuntimeFunctionId::SetIntersection => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // x
                sig.params.push(AbiParam::new(self.pointer_type)); // y
                sig.returns.push(AbiParam::new(self.pointer_type)); // result
                self.register_import("__pasko_set_intersection", sig)
            }
            RuntimeFunctionId::SetDifference => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // x
                sig.params.push(AbiParam::new(self.pointer_type)); // y
                sig.returns.push(AbiParam::new(self.pointer_type)); // result
                self.register_import("__pasko_set_difference", sig)
            }
            RuntimeFunctionId::SetContains => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // set
                sig.params.push(AbiParam::new(self.pointer_type)); // x
                sig.returns.push(AbiParam::new(I8)); // result
                self.register_import("__pasko_set_contains", sig)
            }
            RuntimeFunctionId::SetEqual => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // x
                sig.params.push(AbiParam::new(self.pointer_type)); // y
                sig.returns.push(AbiParam::new(I8)); // result
                self.register_import("__pasko_set_equal", sig)
            }
            RuntimeFunctionId::SetNotEqual => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // x
                sig.params.push(AbiParam::new(self.pointer_type)); // y
                sig.returns.push(AbiParam::new(I8)); // result
                self.register_import("__pasko_set_not_equal", sig)
            }
            RuntimeFunctionId::SetIsSubset => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // x
                sig.params.push(AbiParam::new(self.pointer_type)); // y
                sig.returns.push(AbiParam::new(I8)); // result
                self.register_import("__pasko_set_is_subset", sig)
            }
            RuntimeFunctionId::PointerNew => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // addr to pointer
                sig.params.push(AbiParam::new(I64)); // size in bytes
                self.register_import("__pasko_pointer_new", sig)
            }
            RuntimeFunctionId::PointerDispose => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type)); // addr to pointer
                self.register_import("__pasko_pointer_dispose", sig)
            }
            RuntimeFunctionId::Init => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(I32)); // argc
                sig.params.push(AbiParam::new(self.pointer_type)); // argv
                sig.params.push(AbiParam::new(I32)); // num_program_parameters
                sig.params.push(AbiParam::new(self.pointer_type)); // program_parameters
                sig.params.push(AbiParam::new(I32)); // num_global_files
                sig.params.push(AbiParam::new(self.pointer_type)); // global_files
                self.register_import("__pasko_init", sig)
            }
            RuntimeFunctionId::Finish => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(I32)); // num_global_files
                sig.params.push(AbiParam::new(self.pointer_type)); // global_files
                self.register_import("__pasko_finish", sig)
            }
            RuntimeFunctionId::OutputFile => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.returns.push(AbiParam::new(self.pointer_type));
                self.register_import("__pasko_get_output", sig)
            }
            RuntimeFunctionId::InputFile => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.returns.push(AbiParam::new(self.pointer_type));
                self.register_import("__pasko_get_input", sig)
            }
            RuntimeFunctionId::RewriteFile => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type));
                self.register_import("__pasko_rewrite_file", sig)
            }
            RuntimeFunctionId::RewriteTextfile => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type));
                self.register_import("__pasko_rewrite_textfile", sig)
            }
            RuntimeFunctionId::ResetFile => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type));
                sig.params.push(AbiParam::new(I64)); // bytes
                self.register_import("__pasko_reset_file", sig)
            }
            RuntimeFunctionId::ResetTextfile => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type));
                self.register_import("__pasko_reset_textfile", sig)
            }
            RuntimeFunctionId::PutFile => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type));
                sig.params.push(AbiParam::new(I64)); // bytes
                self.register_import("__pasko_put_file", sig)
            }
            RuntimeFunctionId::PutTextfile => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type));
                self.register_import("__pasko_put_textfile", sig)
            }
            RuntimeFunctionId::GetFile => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type));
                sig.params.push(AbiParam::new(I64)); // bytes
                self.register_import("__pasko_get_file", sig)
            }
            RuntimeFunctionId::GetTextfile => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type));
                self.register_import("__pasko_get_textfile", sig)
            }
            RuntimeFunctionId::BufferVarFile => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(self.pointer_type));
                self.register_import("__pasko_buffer_var_file", sig)
            }
            RuntimeFunctionId::BufferVarTextfile => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type));
                sig.returns.push(AbiParam::new(self.pointer_type));
                self.register_import("__pasko_buffer_var_textfile", sig)
            }
            RuntimeFunctionId::EofFile => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type));
                sig.returns.push(AbiParam::new(I8));
                self.register_import("__pasko_eof_file", sig)
            }
            RuntimeFunctionId::EofTextfile => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type));
                sig.returns.push(AbiParam::new(I8));
                self.register_import("__pasko_eof_textfile", sig)
            }
            RuntimeFunctionId::EolnTextfile => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(self.pointer_type));
                sig.returns.push(AbiParam::new(I8));
                self.register_import("__pasko_eoln_textfile", sig)
            }
            RuntimeFunctionId::SinF64 => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(F64));
                sig.returns.push(AbiParam::new(F64));
                self.register_import("__pasko_sin_f64", sig)
            }
            RuntimeFunctionId::CosF64 => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(F64));
                sig.returns.push(AbiParam::new(F64));
                self.register_import("__pasko_cos_f64", sig)
            }
            RuntimeFunctionId::ExpF64 => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(F64));
                sig.returns.push(AbiParam::new(F64));
                self.register_import("__pasko_exp_f64", sig)
            }
            RuntimeFunctionId::LnF64 => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(F64));
                sig.returns.push(AbiParam::new(F64));
                self.register_import("__pasko_ln_f64", sig)
            }
            RuntimeFunctionId::ArctanF64 => {
                let mut sig = Signature::new(CallConv::SystemV);
                sig.params.push(AbiParam::new(F64));
                sig.returns.push(AbiParam::new(F64));
                self.register_import("__pasko_arctan_f64", sig)
            }
        };
        let import_id = import_id.unwrap();
        self.rt_functions_cache.insert(rtid, import_id);
        import_id
    }

    pub fn type_to_cranelift_type(&self, ty: TypeId) -> cranelift_codegen::ir::Type {
        if self.semantic_context.type_system.is_integer_type(ty)
            || self.semantic_context.type_system.is_enum_type(ty)
        {
            I64
        } else if self.semantic_context.type_system.is_real_type(ty) {
            F64
        } else if self.semantic_context.type_system.is_bool_type(ty) {
            I8
        } else if self.semantic_context.type_system.is_char_type(ty) {
            I32
        } else if self.semantic_context.type_system.is_subrange_type(ty) {
            self.type_to_cranelift_type(self.semantic_context.type_system.get_host_type(ty))
        } else if self.semantic_context.type_system.is_pointer_type(ty) {
            self.pointer_type
        } else {
            panic!(
                "Unexpected type {} when mapping to cranelift type",
                self.semantic_context.type_system.get_type_name(ty)
            );
        }
    }

    pub fn align_to(value: usize, align: usize) -> usize {
        if value % align == 0 {
            value
        } else {
            value + (align - value % align)
        }
    }

    fn size_and_align_in_bytes(&self, ty: TypeId) -> SizeAndAlignment {
        // Query if already cached.
        if let Some(s) = self.size_align_cache.borrow().get(&ty) {
            return s.clone();
        }

        let s = self.size_and_align_in_bytes_impl(ty);

        // Cache result.
        self.size_align_cache.borrow_mut().insert(ty, s.clone());

        s
    }

    fn align_of_variant(&self, variant_part: &Option<VariantPart>) -> usize {
        if let Some(variant) = variant_part {
            // Note that we always storage for the tag, even if it does not have name.
            std::cmp::max(
                self.align_in_bytes(variant.tag_type),
                variant
                    .cases
                    .iter()
                    .map(|case| {
                        std::cmp::max(
                            case.fields
                                .iter()
                                .map(|field| {
                                    let field_sym = self.semantic_context.get_symbol(*field);
                                    let field_sym = field_sym.borrow();
                                    self.align_in_bytes(field_sym.get_type().unwrap())
                                })
                                .max()
                                .unwrap_or(0usize),
                            self.align_of_variant(&case.variant),
                        )
                    })
                    .max()
                    .unwrap_or(0usize),
            )
        } else {
            return 0;
        }
    }

    fn set_offset_of_variant(&self, variant_part: &Option<VariantPart>, offset: usize) -> usize {
        if let Some(variant) = variant_part {
            let mut variant_offset = offset;
            // We assign an offset to the tag type even if it does not have a tag name.
            let tag_type_align = self.align_in_bytes(variant.tag_type);
            let tag_type_size = self.size_in_bytes(variant.tag_type);
            // First pad to the required alignment.
            variant_offset = Self::align_to(variant_offset, tag_type_align);
            // Keep the offset now.
            self.offset_cache
                .borrow_mut()
                .insert(variant.tag_name, offset);
            // Advance the size of the tag.
            variant_offset += tag_type_size;
            let variant_offset = variant_offset;

            variant
                .cases
                .iter()
                .map(|case| {
                    case.fields
                        .iter()
                        .map(|field| {
                            let mut current_case_offset = variant_offset;
                            let field_sym = self.semantic_context.get_symbol(*field);
                            let field_sym = field_sym.borrow();
                            let current_field_align =
                                self.align_in_bytes(field_sym.get_type().unwrap());
                            let current_field_size =
                                self.size_in_bytes(field_sym.get_type().unwrap());

                            // First pad to the required alignment.
                            current_case_offset =
                                Self::align_to(current_case_offset, current_field_align);

                            // Keep the offset now.
                            self.offset_cache
                                .borrow_mut()
                                .insert(*field, current_case_offset);

                            // Advance the size of this field.
                            current_case_offset += current_field_size;

                            self.set_offset_of_variant(&case.variant, current_case_offset);

                            current_case_offset
                        })
                        .max()
                        .unwrap_or(0)
                })
                .max()
                .unwrap_or(0)
        } else {
            0
        }
    }

    fn size_and_align_in_bytes_impl(&self, ty: TypeId) -> SizeAndAlignment {
        if self.semantic_context.type_system.is_real_type(ty)
            || self.semantic_context.type_system.is_integer_type(ty)
            || self.semantic_context.type_system.is_enum_type(ty)
        {
            SizeAndAlignment { size: 8, align: 8 }
        } else if self.semantic_context.type_system.is_char_type(ty) {
            SizeAndAlignment { size: 4, align: 4 }
        } else if self.semantic_context.type_system.is_bool_type(ty) {
            SizeAndAlignment { size: 1, align: 1 }
        } else if self.semantic_context.type_system.is_subrange_type(ty) {
            self.size_and_align_in_bytes(self.semantic_context.type_system.get_host_type(ty))
        } else if self.semantic_context.type_system.is_array_type(ty) {
            let component_type = self
                .semantic_context
                .type_system
                .array_type_get_component_type(ty);
            let component_size = self.size_in_bytes(component_type);
            let align_component = self.align_in_bytes(component_type);
            let index_ty = self
                .semantic_context
                .type_system
                .array_type_get_index_type(ty);
            let index_size = self
                .semantic_context
                .type_system
                .ordinal_type_extent(index_ty);
            if let Some(x) = component_size.checked_mul(index_size as usize) {
                SizeAndAlignment {
                    size: x,
                    align: align_component,
                }
            } else {
                panic!(
                    "Overflow while computing the size of type {}",
                    self.semantic_context.type_system.get_type_name(ty)
                );
            }
        } else if self.semantic_context.type_system.is_record_type(ty) {
            // TODO: we can do something different with packed records (e.g. we
            // could order them by descending alignmeent rather than follow
            // declaration order)
            let mut max_align = 0;
            let fields = self
                .semantic_context
                .type_system
                .record_type_get_fixed_fields(ty);
            for field in fields {
                let field_sym = self.semantic_context.get_symbol(*field);
                let field_sym = field_sym.borrow();
                let current_field_align = self.align_in_bytes(field_sym.get_type().unwrap());
                max_align = std::cmp::max(max_align, current_field_align);
            }
            // Compute the alignment required by the variant, if any.
            let variant = self
                .semantic_context
                .type_system
                .record_type_get_variant_part(ty);
            max_align = std::cmp::max(max_align, self.align_of_variant(variant));
            // Empty structs take at least one byte.
            if max_align == 0 {
                max_align = 1;
            }
            // Rebind immutably.
            let max_align = max_align;

            // Now compute offsets.
            let mut offset = 0usize;
            for field in fields {
                let field_sym = self.semantic_context.get_symbol(*field);
                let field_sym = field_sym.borrow();
                let current_field_align = self.align_in_bytes(field_sym.get_type().unwrap());
                let current_field_size = self.size_in_bytes(field_sym.get_type().unwrap());

                // First pad to the required alignment.
                offset = Self::align_to(offset, current_field_align);

                // Keep the offset now.
                self.offset_cache.borrow_mut().insert(*field, offset);

                // Advance the size of this field.
                offset += current_field_size;
            }

            // Now assign offsets to the variant fields.
            offset += self.set_offset_of_variant(variant, offset);

            // Pad the last element.
            offset = Self::align_to(offset, max_align);

            // Empty structs take at least one byte.
            if offset == 0 {
                offset = 1;
            }

            let final_size = offset;
            SizeAndAlignment {
                size: final_size,
                align: max_align,
            }
        } else if self.semantic_context.type_system.is_set_type(ty) {
            // Sets are opaque reference types, so they take the size and
            // alignment of a pointer.
            SizeAndAlignment {
                size: self.pointer_type.bytes() as usize,
                align: 8,
            }
        } else if self.semantic_context.type_system.is_pointer_type(ty) {
            SizeAndAlignment {
                size: self.pointer_type.bytes() as usize,
                align: 8,
            }
        } else if self.semantic_context.type_system.is_file_type(ty) {
            // Sets are opaque reference types, so they take the size and
            // alignment of a pointer.
            SizeAndAlignment {
                size: self.pointer_type.bytes() as usize,
                align: 8,
            }
        } else {
            panic!(
                "Unexpected size request for type {}",
                self.semantic_context.type_system.get_type_name(ty)
            );
        }
    }

    pub fn size_in_bytes(&self, ty: TypeId) -> usize {
        self.size_and_align_in_bytes(ty).size
    }

    pub fn align_in_bytes(&self, ty: TypeId) -> usize {
        self.size_and_align_in_bytes(ty).align
    }

    fn common_function_emisson(
        &mut self,
        function_name: &String,
        function_symbol_id: SymbolId,
        return_symbol_id: Option<SymbolId>,
        block: &span::SpannedBox<ast::Block>,
    ) {
        let mut sig = Signature::new(CallConv::SystemV);

        let mut return_type_id_not_simple = None;
        let mut return_symbol_info = None;
        if let Some(return_symbol_id) = return_symbol_id {
            let return_symbol = self.semantic_context.get_symbol(return_symbol_id);
            return_symbol_info = Some((return_symbol_id, return_symbol.clone()));
            let return_symbol = return_symbol.borrow();
            let return_symbol_type_id = return_symbol.get_type().unwrap();
            if self
                .semantic_context
                .type_system
                .is_simple_type(return_symbol_type_id)
                || self
                    .semantic_context
                    .type_system
                    .is_pointer_type(return_symbol_type_id)
            {
                sig.returns.push(AbiParam::new(
                    self.type_to_cranelift_type(return_symbol_type_id),
                ));
            } else if self
                .semantic_context
                .type_system
                .is_array_type(return_symbol_type_id)
                || self
                    .semantic_context
                    .type_system
                    .is_record_type(return_symbol_type_id)
                || self
                    .semantic_context
                    .type_system
                    .is_set_type(return_symbol_type_id)
            {
                // We pass it as a pointer parameter.
                sig.params.push(AbiParam::new(self.pointer_type));
                return_type_id_not_simple = Some(return_symbol_type_id);
            } else {
                panic!(
                    "Unexpected return type {} while lowering to cranelift",
                    self.semantic_context
                        .type_system
                        .get_type_name(return_symbol_type_id)
                )
            }
        }

        // Remove mutability.
        let return_symbol_info = return_symbol_info;
        let return_type_id_not_simple = return_type_id_not_simple;
        let return_type_is_simple = return_type_id_not_simple.is_none();

        let function_symbol = self.semantic_context.get_symbol(function_symbol_id);
        let function_symbol = function_symbol.borrow();
        let params: Vec<_> = function_symbol
            .get_formal_parameters()
            .unwrap()
            .iter()
            .map(|sym_id| {
                let sym_id = *sym_id;
                let sym = self.semantic_context.get_symbol(sym_id);
                let sym_type = sym.borrow().get_type().unwrap();
                (sym_id, sym, sym_type)
            })
            .collect();

        let mut parameters_to_dispose = vec![];
        for (param_symbol_id, param_symbol, ..) in params.iter() {
            let param_symbol = param_symbol.borrow();
            match param_symbol.get_parameter().unwrap() {
                pasko_frontend::symbol::ParameterKind::Value => {
                    let param_symbol_type_id = param_symbol.get_type().unwrap();
                    if self
                        .semantic_context
                        .type_system
                        .is_simple_type(param_symbol_type_id)
                    {
                        sig.params.push(AbiParam::new(
                            self.type_to_cranelift_type(param_symbol_type_id),
                        ));
                    } else if self
                        .semantic_context
                        .type_system
                        .is_array_type(param_symbol_type_id)
                        || self
                            .semantic_context
                            .type_system
                            .is_record_type(param_symbol_type_id)
                        || self
                            .semantic_context
                            .type_system
                            .is_set_type(param_symbol_type_id)
                        || self
                            .semantic_context
                            .type_system
                            .is_pointer_type(param_symbol_type_id)
                    {
                        sig.params.push(AbiParam::new(self.pointer_type));
                        if self.type_contains_set_types(param_symbol_type_id) {
                            parameters_to_dispose.push(*param_symbol_id);
                        }
                    } else {
                        panic!(
                            "Unexpected parameter type {} while lowering to cranelift",
                            self.semantic_context
                                .type_system
                                .get_type_name(param_symbol_type_id)
                        )
                    }
                }
                pasko_frontend::symbol::ParameterKind::Variable => {
                    sig.params.push(AbiParam::new(self.pointer_type));
                }
            }
        }
        // Remove mutability;
        let parameters_to_dispose = parameters_to_dispose;

        let func_id = self
            .object_module
            .as_mut()
            .unwrap()
            .declare_function(function_name, Linkage::Local, &sig)
            .unwrap();

        self.function_identifiers
            .insert(function_symbol_id, func_id);
        self.function_names.insert(func_id, function_name.clone());

        let mut func = Function::with_name_signature(UserFuncName::user(0, func_id.as_u32()), sig);
        let mut func_builder_ctx = FunctionBuilderContext::new();
        let builder = FunctionBuilder::new(&mut func, &mut func_builder_ctx);

        let mut function_codegen = FunctionCodegenVisitor::new(self, Some(builder));

        function_codegen.init_function();
        // We are now in the entry block and we have access to the parameters.

        // Allocate the return value in the stack only if simple.
        if let Some(return_symbol_id) = return_symbol_id {
            if return_type_is_simple {
                function_codegen.allocate_value_in_stack(return_symbol_id);
            }
        }

        let effective_params: Vec<_> = if return_type_is_simple {
            params.into_iter().map(|x| (x.0, x.1, x.2, false)).collect()
        } else {
            // Handle this special case by prepending the result type info.
            let return_symbol_info = return_symbol_info.unwrap();
            vec![(
                return_symbol_info.0,
                return_symbol_info.1,
                return_type_id_not_simple.unwrap(),
                true,
            )]
            .into_iter()
            .chain(params.into_iter().map(|x| (x.0, x.1, x.2, false)))
            .collect()
        };

        // Allocate parameters in the stack
        effective_params
            .iter()
            .for_each(|(param_sym_id, param_sym, param_type_id, is_return)| {
                let is_simple_type = function_codegen
                    .get_codegen()
                    .semantic_context
                    .type_system
                    .is_simple_type(*param_type_id);
                let param_sym = param_sym.borrow();
                if let Some(parameter_kind) = param_sym.get_parameter() {
                    assert!(!is_return);
                    let is_set_type = function_codegen
                        .get_codegen()
                        .semantic_context
                        .type_system
                        .is_set_type(*param_type_id);
                    let is_pointer_type = function_codegen
                        .get_codegen()
                        .semantic_context
                        .type_system
                        .is_pointer_type(*param_type_id);
                    match parameter_kind {
                        ParameterKind::Value => {
                            if is_simple_type || is_set_type || is_pointer_type {
                                // Simple types passed by value will have their
                                // actual value in the stack.
                                //
                                // Set types are opaque, so their value is their
                                // opaque pointer. The caller will be
                                // responsible for creating a new opaque pointer
                                // to honour value semantics.
                                //
                                // Pointer types can be passed by value in
                                // cranelift as well.
                                function_codegen.allocate_value_in_stack(*param_sym_id);
                            } else {
                                // Other non-simple types passed by value are
                                // non-opaque storage-wise so we pass a pointer
                                // to their actual "by value" storage (allocated
                                // by the caller).
                                function_codegen.allocate_address_in_stack(*param_sym_id);
                            }
                        }
                        ParameterKind::Variable => {
                            // Variable parameters are always passed as pointer to variable itself.
                            function_codegen.allocate_address_in_stack(*param_sym_id);
                        }
                    }
                } else {
                    assert!(is_return);
                    assert!(!is_simple_type, "This should be a non simple return");
                    function_codegen.allocate_address_in_stack(*param_sym_id);
                }
            });
        // Copy them in.
        effective_params
            .iter()
            .enumerate()
            .for_each(|(idx, (param_sym_id, ..))| {
                function_codegen.copy_in_function_parameter(idx, *param_sym_id);
            });
        // Notify about parameters that require disposing.
        parameters_to_dispose
            .iter()
            .for_each(|sym| function_codegen.add_symbol_to_dispose(*sym));

        // Generate code for the block of the function.
        block
            .get()
            .walk_mut(&mut function_codegen, block.loc(), block.id());

        // Now free the local stuff that needs to be freed.
        function_codegen.dispose_symbols();

        // Return the value
        if let Some(return_symbol_id) = return_symbol_id {
            if return_type_is_simple {
                let ret_value = function_codegen.load_symbol_from_stack(return_symbol_id);
                function_codegen.builder().ins().return_(&[ret_value]);
            } else {
                function_codegen.builder().ins().return_(&[]);
            }
        } else {
            function_codegen.builder().ins().return_(&[]);
        }

        function_codegen.finish_function();

        // Verify the IR
        self.verify_ir(&func, function_name);

        self.annotations.clear_temporary();

        // Codegen the IR it to the module.
        self.ctx.clear();
        self.ctx.func = func;
        self.object_module
            .as_mut()
            .unwrap()
            .define_function(func_id, &mut self.ctx)
            .unwrap();
    }

    fn add_global_to_dispose(&mut self, sym: SymbolId) {
        debug_assert!(
            self.globals_to_dispose.iter().find(|&&s| s == sym) == None,
            "adding global symbol more than once for disposal"
        );
        self.globals_to_dispose.push(sym);
    }

    fn variant_contains_set_types(&self, variant: &Option<VariantPart>) -> bool {
        if let Some(variant) = variant {
            variant.cases.iter().any(|case| {
                case.fields.iter().any(|field| {
                    let field_sym = self.semantic_context.get_symbol(*field);
                    let field_sym = field_sym.borrow();
                    let field_ty = field_sym.get_type().unwrap();
                    self.type_contains_set_types(field_ty)
                }) || self.variant_contains_set_types(&case.variant)
            })
        } else {
            false
        }
    }

    fn type_contains_set_types_impl(&self, ty: TypeId) -> bool {
        let ts = &self.semantic_context.type_system;
        if ts.is_set_type(ty) {
            true
        } else if ts.is_array_type(ty) {
            self.type_contains_set_types(ts.array_type_get_component_type(ty))
        } else if ts.is_record_type(ty) {
            let fields = ts.record_type_get_fixed_fields(ty);
            let fixed_fields = fields.iter().any(|&field| {
                let field_sym = self.semantic_context.get_symbol(field);
                let field_sym = field_sym.borrow();
                let field_ty = field_sym.get_type().unwrap();
                self.type_contains_set_types(field_ty)
            });
            let variant_part = self.variant_contains_set_types(ts.record_type_get_variant_part(ty));
            if variant_part {
                unimplemented!("Variants with set types");
            }
            fixed_fields || variant_part
        } else {
            false
        }
    }

    pub fn type_contains_set_types(&self, ty: TypeId) -> bool {
        if let Some(&s) = self.trivially_copiable_cache.borrow().get(&ty) {
            return s;
        }
        let r = self.type_contains_set_types_impl(ty);

        self.trivially_copiable_cache.borrow_mut().insert(ty, r);

        r
    }

    pub fn register_input_output(&mut self) {
        let input_output: Vec<_> = (0..2)
            .map(|_x| {
                let data_id = self
                    .object_module
                    .as_mut()
                    .unwrap()
                    .declare_anonymous_data(true, false)
                    .unwrap();

                let mut data_desc = DataDescription::new();
                let size_in_bytes = self.pointer_type.bytes();
                data_desc.define_zeroinit(size_in_bytes as usize);
                data_desc.set_align(size_in_bytes as u64);

                self.object_module
                    .as_mut()
                    .unwrap()
                    .define_data(data_id, &data_desc)
                    .unwrap();
                data_id
            })
            .collect();

        self.input_data_id = Some(input_output[0]);
        self.global_names
            .insert(input_output[0], "[input-textfile]".to_string());

        self.output_data_id = Some(input_output[1]);
        self.global_names
            .insert(input_output[1], "[output-textfile]".to_string());
    }

    pub fn get_input_file_data_id(&self) -> cranelift_module::DataId {
        self.input_data_id.unwrap()
    }

    pub fn get_output_file_data_id(&self) -> cranelift_module::DataId {
        self.output_data_id.unwrap()
    }

    fn verify_ir(&self, func: &cranelift_codegen::ir::Function, function_name: &str) {
        let flags = settings::Flags::new(settings::builder());
        let res = verify_function(&func, &flags);
        if self.ir_dump {
            println!("*** IR for '{}'", function_name);
            let mut s = String::new();
            let mut my_writer = AnnotatedFuncWriter::new(&self.annotations);
            cranelift_codegen::write::decorate_function(&mut my_writer, &mut s, func).unwrap();
            println!("{}", s.trim());
        }
        if let Err(errors) = res {
            panic!("{}", errors);
        }
        if self.ir_dump {
            println!("*** IR for '{}' seems OK\n", function_name);
        }
    }
}

impl<'a> VisitorMut for CodegenVisitor<'a> {
    fn visit_pre_program(
        &mut self,
        n: &ast::Program,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        // ProgramHeading is ignored
        n.1.get().walk_mut(self, n.1.loc(), n.1.id());

        false
    }

    fn visit_pre_program_block(
        &mut self,
        n: &ast::ProgramBlock,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        let block = n.0.get(); // Block

        let _labels = &block.0;
        let _constants = &block.1;
        let _types = &block.2;
        let variables = &block.3;
        let procedures = &block.4;
        let statements = &block.5;

        // Create the IR for the global variables.
        if let Some(variables) = variables {
            variables
                .get()
                .walk_mut(self, variables.loc(), variables.id());
        }

        // Create input and output as standard files.
        // TODO: We only have to do this if input, output appear in program.
        self.register_input_output();

        // Functions and procedures
        if let Some(procedures) = procedures {
            procedures
                .get()
                .walk_mut(self, procedures.loc(), procedures.id());
        }

        // Main program
        let mut sig = Signature::new(CallConv::SystemV);
        sig.returns.push(AbiParam::new(I32));
        sig.params.push(AbiParam::new(I32)); // argc
        sig.params.push(AbiParam::new(self.pointer_type)); // argv

        let func_id = self
            .object_module
            .as_mut()
            .unwrap()
            .declare_function("main", Linkage::Export, &sig)
            .unwrap();
        let mut func = Function::with_name_signature(UserFuncName::user(0, func_id.as_u32()), sig);

        // Obtain early reference to __pasko_init and __pasko_finish
        let pasko_init_id = self.get_runtime_function(RuntimeFunctionId::Init);
        let pasko_init_func_ref = self
            .object_module
            .as_mut()
            .unwrap()
            .declare_func_in_func(pasko_init_id, &mut func);
        self.annotations
            .new_function_ref(pasko_init_func_ref, "__pasko_init");

        let pasko_finish_id = self.get_runtime_function(RuntimeFunctionId::Finish);
        let pasko_finish_func_ref = self
            .object_module
            .as_mut()
            .unwrap()
            .declare_func_in_func(pasko_finish_id, &mut func);
        self.annotations
            .new_function_ref(pasko_finish_func_ref, "__pasko_finish");

        let get_input_func_id = self.get_runtime_function(RuntimeFunctionId::InputFile);
        let get_input_func_ref = self
            .object_module
            .as_mut()
            .unwrap()
            .declare_func_in_func(get_input_func_id, &mut func);
        self.annotations
            .new_function_ref(get_input_func_ref, "__pasko_get_input");

        let get_output_func_id = self.get_runtime_function(RuntimeFunctionId::OutputFile);
        let get_output_func_ref = self
            .object_module
            .as_mut()
            .unwrap()
            .declare_func_in_func(get_output_func_id, &mut func);
        self.annotations
            .new_function_ref(get_output_func_ref, "__pasko_get_output");

        let input_data_id = self.input_data_id.unwrap();
        let output_data_id = self.output_data_id.unwrap();
        let pointer_type = self.pointer_type;

        let mut func_builder_ctx = FunctionBuilderContext::new();
        let builder = FunctionBuilder::new(&mut func, &mut func_builder_ctx);

        let globals_to_dispose = std::mem::take(&mut self.globals_to_dispose);

        let program_parameters = self.semantic_context.program_parameters.clone();
        let num_program_params = program_parameters.len() as i64;
        let global_files = self.semantic_context.global_files.clone();

        let mut function_codegen = FunctionCodegenVisitor::new(self, Some(builder));

        function_codegen.init_function();
        globals_to_dispose
            .iter()
            .for_each(|&sym| function_codegen.add_symbol_to_dispose(sym));

        // Call __pasko_init
        let entry_block = function_codegen.get_entry_block().unwrap();
        let block_params = function_codegen.builder().block_params(entry_block);
        let argc = block_params[0];
        let argv = block_params[1];
        let num_program_params = function_codegen
            .builder()
            .ins()
            .iconst(I32, num_program_params);

        let program_params_addr = {
            let str_addresses: Vec<_> = program_parameters
                .iter()
                .map(|(s, _)| {
                    let addr_string = function_codegen.emit_string_literal(s);
                    addr_string
                })
                .collect();
            function_codegen
                .emit_stack_ptr_array_null_ended(&str_addresses, "program-parameter-names")
        };

        let global_files_addr = {
            let object_addresses: Vec<_> = global_files
                .iter()
                .map(|sym_id| function_codegen.get_address_of_symbol(*sym_id))
                .collect();
            function_codegen.emit_stack_ptr_array_null_ended(&object_addresses, "global-files")
        };

        let num_global_files = function_codegen
            .builder()
            .ins()
            .iconst(I32, global_files.len() as i64);

        function_codegen.builder().ins().call(
            pasko_init_func_ref,
            &[
                argc,
                argv,
                num_program_params,
                program_params_addr,
                num_global_files,
                global_files_addr,
            ],
        );

        // Store the values of input and output.
        let call = function_codegen
            .builder()
            .ins()
            .call(get_input_func_ref, &[]);
        let result = {
            let results = function_codegen.builder().inst_results(call);
            assert!(results.len() == 1, "Invalid number of results");
            results[0]
        };
        let input_addr_gv = function_codegen.get_global_value(input_data_id);
        let input_addr = function_codegen
            .builder()
            .ins()
            .global_value(pointer_type, input_addr_gv);
        function_codegen.builder().ins().store(
            cranelift_codegen::ir::MemFlags::new(),
            result,
            input_addr,
            0,
        );
        let call = function_codegen
            .builder()
            .ins()
            .call(get_output_func_ref, &[]);
        let result = {
            let results = function_codegen.builder().inst_results(call);
            assert!(results.len() == 1, "Invalid number of results");
            results[0]
        };
        let output_addr_gv = function_codegen.get_global_value(output_data_id);
        let output_addr = function_codegen
            .builder()
            .ins()
            .global_value(pointer_type, output_addr_gv);
        function_codegen.builder().ins().store(
            cranelift_codegen::ir::MemFlags::new(),
            result,
            output_addr,
            0,
        );

        // Create the IR for the statements.
        statements
            .get()
            .walk_mut(&mut function_codegen, n.0.loc(), n.0.id());

        // If everything went well we should be in the return block.
        assert!(function_codegen.is_top_level_block());

        // Now free the global stuff that needs to be freed.
        function_codegen.dispose_symbols();

        // And now finalize the runtime.
        function_codegen.builder().ins().call(
            pasko_finish_func_ref,
            &[num_global_files, global_files_addr],
        );

        // So return 0 because this is a well behaved main.
        let zero = function_codegen.builder().ins().iconst(I32, 0);
        function_codegen.builder().ins().return_(&[zero]);

        function_codegen.finish_function();

        // Verify the IR
        self.verify_ir(&func, "main");

        // Codegen the IR it to the module.
        self.ctx.clear();
        self.ctx.func = func;
        self.object_module
            .as_mut()
            .unwrap()
            .define_function(func_id, &mut self.ctx)
            .unwrap();

        false
    }

    fn visit_pre_procedure_definition(
        &mut self,
        n: &ast::ProcedureDefinition,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        self.common_function_emisson(
            n.0.get(),
            self.semantic_context.get_ast_symbol(n.0.id()).unwrap(),
            None,
            &n.2,
        );
        false
    }

    fn visit_pre_function_definition(
        &mut self,
        n: &ast::FunctionDefinition,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        let return_symbol_id = {
            let function_symbol_id = self.semantic_context.get_ast_symbol(n.0.id()).unwrap();
            let function_symbol = self.semantic_context.get_symbol(function_symbol_id);
            let function_symbol = function_symbol.borrow();
            function_symbol.get_return_symbol().unwrap()
        };
        self.common_function_emisson(
            n.0.get(),
            self.semantic_context.get_ast_symbol(n.0.id()).unwrap(),
            Some(return_symbol_id),
            &n.3,
        );
        false
    }

    fn visit_pre_variable_declaration(
        &mut self,
        n: &ast::VariableDeclaration,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        for sym in n.0.iter() {
            let sym_id = self.semantic_context.get_ast_symbol(sym.id()).unwrap();
            let sym = self.semantic_context.get_symbol(sym_id);
            let sym = sym.borrow();
            let ty = sym.get_type().unwrap();

            if self.semantic_context.type_system.is_integer_type(ty)
                || self.semantic_context.type_system.is_real_type(ty)
                || self.semantic_context.type_system.is_bool_type(ty)
                || self.semantic_context.type_system.is_char_type(ty)
                || self.semantic_context.type_system.is_enum_type(ty)
                || self.semantic_context.type_system.is_subrange_type(ty)
                || self.semantic_context.type_system.is_array_type(ty)
                || self.semantic_context.type_system.is_record_type(ty)
                || self.semantic_context.type_system.is_set_type(ty)
                || self.semantic_context.type_system.is_pointer_type(ty)
                || self.semantic_context.type_system.is_file_type(ty)
            {
                let data_id = self
                    .object_module
                    .as_mut()
                    .unwrap()
                    .declare_anonymous_data(true, false)
                    .unwrap();
                self.global_names.insert(data_id, sym.get_name().clone());

                let mut data_desc = DataDescription::new();
                let size_in_bytes = self.size_in_bytes(ty);
                data_desc.define_zeroinit(size_in_bytes);
                data_desc.set_align(self.align_in_bytes(ty) as u64);

                self.object_module
                    .as_mut()
                    .unwrap()
                    .define_data(data_id, &data_desc)
                    .unwrap();

                self.data_location
                    .insert(sym_id, DataLocation::GlobalVar(data_id));

                if self.type_contains_set_types(ty) {
                    self.add_global_to_dispose(sym_id);
                }
            } else {
                panic!(
                    "Unexpected type {} in variable declaration",
                    self.semantic_context.type_system.get_type_name(ty)
                );
            }
        }

        false
    }
}

#![allow(unused_imports)]

use cranelift_codegen::settings::Configurable;
use pasko_frontend::ast::{self, FormalParameter};
use pasko_frontend::semantic::SemanticContext;
use pasko_frontend::span;
use pasko_frontend::symbol::{ParameterKind, SymbolId};
use pasko_frontend::typesystem::TypeId;
use pasko_frontend::visitor::{Visitable, VisitorMut};

use cranelift_codegen::ir::types::{F64, I32, I64, I8};
use cranelift_codegen::ir::{
    function, AbiParam, Function, Signature, StackSlotData, StackSlotKind, UserFuncName,
};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{DataDescription, Module};

use cranelift_codegen::verifier::verify_function;

use cranelift_codegen::ir::InstBuilder;
use cranelift_module::Linkage;
use cranelift_object;

use std::any::Any;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

use crate::datalocation::DataLocation;
use crate::function::FunctionCodegenVisitor;
use crate::runtime::RuntimeFunctions;

pub struct CodegenVisitor<'a> {
    pub object_module: Option<Box<cranelift_object::ObjectModule>>,
    pub ctx: cranelift_codegen::Context,
    pub semantic_context: &'a SemanticContext,
    pub pointer_type: cranelift_codegen::ir::Type,
    pub rt: RuntimeFunctions,
    pub string_table: HashMap<String, cranelift_module::DataId>,

    pub data_location: HashMap<SymbolId, DataLocation>,
    pub function_identifiers: HashMap<SymbolId, cranelift_module::FuncId>,

    ir_dump: bool,
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

        let mut visitor = CodegenVisitor {
            object_module: Some(Box::new(object_module)),
            ctx,
            semantic_context,
            pointer_type,
            rt: RuntimeFunctions::default(),
            string_table: HashMap::new(),
            data_location: HashMap::new(),
            function_identifiers: HashMap::new(),
            // Private
            ir_dump,
        };

        visitor.initialize_module();

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
        Some(func_id)
    }

    fn initialize_module(&mut self) {
        let mut sig = Signature::new(CallConv::SystemV);
        sig.params.push(AbiParam::new(self.pointer_type)); // string
        self.rt.write_str = self.register_import("__pasko_write_str", sig);

        let mut sig = Signature::new(CallConv::SystemV);
        sig.params.push(AbiParam::new(I64)); // number
        sig.params.push(AbiParam::new(I32)); // total_width
        self.rt.write_i64 = self.register_import("__pasko_write_i64", sig);

        let mut sig = Signature::new(CallConv::SystemV);
        sig.params.push(AbiParam::new(F64)); // number
        sig.params.push(AbiParam::new(I32)); // total_width
        sig.params.push(AbiParam::new(I32)); // frac_digits
        self.rt.write_f64 = self.register_import("__pasko_write_f64", sig);

        let mut sig = Signature::new(CallConv::SystemV);
        sig.params.push(AbiParam::new(I8)); // number
        self.rt.write_bool = self.register_import("__pasko_write_bool", sig);

        let sig = Signature::new(CallConv::SystemV);
        self.rt.write_newline = self.register_import("__pasko_write_newline", sig);

        let mut sig = Signature::new(CallConv::SystemV);
        sig.returns.push(AbiParam::new(I64));
        self.rt.read_i64 = self.register_import("__pasko_read_i64", sig);

        let mut sig = Signature::new(CallConv::SystemV);
        sig.returns.push(AbiParam::new(F64));
        self.rt.read_f64 = self.register_import("__pasko_read_f64", sig);

        let sig = Signature::new(CallConv::SystemV);
        self.rt.read_newline = self.register_import("__pasko_read_newline", sig);
    }

    pub fn type_to_cranelift_type(&self, ty: TypeId) -> cranelift_codegen::ir::Type {
        if self.semantic_context.is_integer_type(ty) || self.semantic_context.is_enum_type(ty) {
            I64
        } else if self.semantic_context.is_real_type(ty) {
            F64
        } else if self.semantic_context.is_bool_type(ty) {
            I8
        } else if self.semantic_context.is_subrange_type(ty) {
            self.type_to_cranelift_type(self.semantic_context.get_host_type(ty))
        } else {
            panic!(
                "Unexpected type {} when mapping to cranelift type",
                self.semantic_context.get_type_name(ty)
            );
        }
    }

    fn common_function_emisson(
        &mut self,
        function_name: &String,
        function_symbol_id: SymbolId,
        return_symbol_id: Option<SymbolId>,
        block: &span::SpannedBox<ast::Block>,
    ) {
        let mut sig = Signature::new(CallConv::SystemV);

        if let Some(return_symbol_id) = return_symbol_id {
            let return_symbol = self.semantic_context.get_symbol(return_symbol_id);
            let return_symbol_type_id = return_symbol.get_type().unwrap();
            sig.returns.push(AbiParam::new(
                self.type_to_cranelift_type(return_symbol_type_id),
            ));
        }

        let function_symbol = self.semantic_context.get_symbol(function_symbol_id);
        let params: Vec<_> = function_symbol
            .get_formal_parameters()
            .unwrap()
            .iter()
            .map(|sym_id| (*sym_id, self.semantic_context.get_symbol(*sym_id)))
            .collect();

        for (_param_symbol_id, param_symbol) in params.iter() {
            match param_symbol.get_parameter().unwrap() {
                pasko_frontend::symbol::ParameterKind::Value => {
                    let param_symbol_type_id = param_symbol.get_type().unwrap();
                    sig.params.push(AbiParam::new(
                        self.type_to_cranelift_type(param_symbol_type_id),
                    ));
                }
                pasko_frontend::symbol::ParameterKind::Variable => {
                    sig.params.push(AbiParam::new(self.pointer_type));
                }
            }
        }

        let func_id = self
            .object_module
            .as_mut()
            .unwrap()
            .declare_function(function_name, Linkage::Local, &sig)
            .unwrap();

        self.function_identifiers
            .insert(function_symbol_id, func_id);

        let mut func = Function::with_name_signature(UserFuncName::user(0, func_id.as_u32()), sig);
        let mut func_builder_ctx = FunctionBuilderContext::new();
        let builder = FunctionBuilder::new(&mut func, &mut func_builder_ctx);

        let mut function_codegen = FunctionCodegenVisitor::new(self, Some(builder));

        function_codegen.init_function();
        // We are now in the entry block and we have access to the parameters.

        // Allocate the return value in the stack
        if let Some(return_symbol_id) = return_symbol_id {
            function_codegen.allocate_value_in_stack(return_symbol_id);
        }

        // Allocate parameters in the stack
        params.iter().for_each(|(param_sym_id, param_sym)| {
            let parameter_kind = param_sym.get_parameter().unwrap();
            match parameter_kind {
                ParameterKind::Value => {
                    function_codegen.allocate_value_in_stack(*param_sym_id);
                }
                ParameterKind::Variable => {
                    function_codegen.allocate_address_in_stack(*param_sym_id);
                }
            }
        });
        // Copy them in.
        params
            .iter()
            .enumerate()
            .for_each(|(idx, (param_sym_id, _param_sym))| {
                function_codegen.copy_in_function_parameter(idx, *param_sym_id);
            });

        // Generate code for the block of the function.
        block
            .get()
            .walk_mut(&mut function_codegen, block.loc(), block.id());

        // Return the value
        if let Some(return_symbol_id) = return_symbol_id {
            let ret_value = function_codegen.load_symbol_from_stack(return_symbol_id);
            function_codegen.builder().ins().return_(&[ret_value]);
        } else {
            function_codegen.builder().ins().return_(&[]);
        }

        function_codegen.finish_function();

        // Verify the IR
        let flags = settings::Flags::new(settings::builder());
        let res = verify_function(&func, &flags);
        if self.ir_dump {
            println!(
                "*** IR for {} '{}'",
                if return_symbol_id.is_some() {
                    "function"
                } else {
                    "procedure"
                },
                function_name
            );
            let s = format!("{}", func.display());
            println!("{}", s.trim());
        }
        if let Err(errors) = res {
            panic!("{}", errors);
        }
        if self.ir_dump {
            println!(
                "*** IR for {} '{}' seems OK\n",
                if return_symbol_id.is_some() {
                    "function"
                } else {
                    "procedure"
                },
                function_name
            );
        }

        // Codegen the IR it to the module.
        self.ctx.clear();
        self.ctx.func = func;
        self.object_module
            .as_mut()
            .unwrap()
            .define_function(func_id, &mut self.ctx)
            .unwrap();
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

        // Functions and procedures
        if let Some(procedures) = procedures {
            procedures
                .get()
                .walk_mut(self, procedures.loc(), procedures.id());
        }

        // Main program
        let mut sig = Signature::new(CallConv::SystemV);
        // argc
        sig.returns.push(AbiParam::new(I32));
        // argv
        sig.params.push(AbiParam::new(I32)); // argc
        sig.params.push(AbiParam::new(self.pointer_type)); // argv

        let func_id = self
            .object_module
            .as_mut()
            .unwrap()
            .declare_function("main", Linkage::Export, &sig)
            .unwrap();

        let mut func = Function::with_name_signature(UserFuncName::user(0, func_id.as_u32()), sig);
        let mut func_builder_ctx = FunctionBuilderContext::new();
        let builder = FunctionBuilder::new(&mut func, &mut func_builder_ctx);

        let mut function_codegen = FunctionCodegenVisitor::new(self, Some(builder));

        function_codegen.init_function();

        // Create the IR for the statements.
        statements
            .get()
            .walk_mut(&mut function_codegen, n.0.loc(), n.0.id());

        // If everything went well we should be in the return block.
        assert!(function_codegen.is_top_level_block());
        // So return 0 because this is a well behaved main.
        let zero = function_codegen.builder().ins().iconst(I32, 0);
        function_codegen.builder().ins().return_(&[zero]);

        function_codegen.finish_function();

        // Verify the IR
        let flags = settings::Flags::new(settings::builder());
        let res = verify_function(&func, &flags);
        if self.ir_dump {
            println!("*** IR for main");
            let s = format!("{}", func.display());
            println!("{}", s.trim());
        }
        if let Err(errors) = res {
            panic!("{}", errors);
        }
        if self.ir_dump {
            println!("*** IR for main seems OK\n");
        }

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
            let ty = sym.get_type().unwrap();

            if self.semantic_context.is_integer_type(ty)
                || self.semantic_context.is_real_type(ty)
                || self.semantic_context.is_bool_type(ty)
                || self.semantic_context.is_enum_type(ty)
                || self.semantic_context.is_subrange_type(ty)
            {
                let data_id = self
                    .object_module
                    .as_mut()
                    .unwrap()
                    .declare_anonymous_data(true, false)
                    .unwrap();

                let mut data_desc = DataDescription::new();
                let size_in_bytes = self.semantic_context.size_in_bytes(ty);
                data_desc.define_zeroinit(size_in_bytes);

                self.object_module
                    .as_mut()
                    .unwrap()
                    .define_data(data_id, &data_desc)
                    .unwrap();

                self.data_location
                    .insert(sym_id, DataLocation::GlobalVar(data_id));
            } else {
                panic!(
                    "Unexpected type {} in variable declaration",
                    self.semantic_context.get_type_name(ty)
                );
            }
        }

        false
    }
}

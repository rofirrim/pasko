#![allow(unused_imports)]

use cranelift_codegen::ir::StackSlot;
use cranelift_codegen::settings::Configurable;
use cranelift_object::object::read::elf::ElfSectionIterator32;
use cranelift_object::object::SymbolKind;
use pasko_frontend::ast;
use pasko_frontend::ast::ExprVariable;
use pasko_frontend::ast::UnaryOp;
use pasko_frontend::constant::Constant;
use pasko_frontend::semantic;
use pasko_frontend::semantic::SemanticContext;
use pasko_frontend::span;
use pasko_frontend::symbol;
use pasko_frontend::symbol::ParameterKind;
use pasko_frontend::typesystem::TypeId;
use pasko_frontend::visitor::{Visitable, VisitorMut};

use cranelift_codegen::entity::EntityRef;
use cranelift_codegen::ir::types::{F64, I32, I64, I8};
use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::ir::{AbiParam, Function, Signature, UserFuncName};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::Module;

use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::stackslot::{StackSlotData, StackSlotKind};
use cranelift_codegen::verifier::verify_function;

use cranelift_module::Linkage;
use cranelift_object; // ::{ObjectBuilder, ObjectModule};

use std::array;
use std::collections::btree_map::VacantEntry;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::thread::current;

use crate::datalocation::DataLocation;
use crate::program::CodegenVisitor;

#[derive(Clone)]
struct Temporary {
    addr: cranelift_codegen::ir::Value,
    ty: pasko_frontend::typesystem::TypeId,
}

pub struct FunctionCodegenVisitor<'a, 'b, 'c> {
    codegen: &'a mut CodegenVisitor<'b>,
    builder_obj: Option<FunctionBuilder<'c>>,

    block_stack: Vec<cranelift_codegen::ir::Block>,

    value_map: HashMap<span::SpanId, cranelift_codegen::ir::Value>,

    var_counter: usize,
    entry_block: Option<cranelift_codegen::ir::Block>,

    data_references: HashMap<cranelift_module::DataId, cranelift_codegen::ir::GlobalValue>,
    function_references: HashMap<cranelift_module::FuncId, cranelift_codegen::ir::FuncRef>,

    temporaries_to_dispose: Vec<Temporary>,

    symbols_to_dispose: Vec<pasko_frontend::symbol::SymbolId>,
}

impl<'a, 'b, 'c> FunctionCodegenVisitor<'a, 'b, 'c> {
    pub fn init_function(&mut self) {
        let entry_block = self.builder().create_block();
        self.entry_block = Some(entry_block);
        self.builder()
            .append_block_params_for_function_params(entry_block);

        self.block_stack.push(entry_block);
        self.builder().switch_to_block(entry_block);
    }

    pub fn get_codegen(&mut self) -> &mut CodegenVisitor<'b> {
        self.codegen
    }

    pub fn finish_function(&mut self) {
        self.block_stack.pop();
        assert!(self.block_stack.len() == 0);

        let mut builder = self.builder_obj.take().unwrap();

        builder.seal_all_blocks();
        builder.finalize();
    }

    pub fn builder(&mut self) -> &mut FunctionBuilder<'c> {
        self.builder_obj.as_mut().unwrap()
    }

    fn set_value(&mut self, id: span::SpanId, value: cranelift_codegen::ir::Value) {
        self.value_map.insert(id, value);
    }

    fn get_value(&self, id: span::SpanId) -> cranelift_codegen::ir::Value {
        *self.value_map.get(&id).unwrap()
    }

    pub fn is_top_level_block(&self) -> bool {
        self.block_stack.len() == 1
    }

    pub fn emit_const_char(&mut self, i: u32) -> cranelift_codegen::ir::Value {
        self.builder().ins().iconst(I32, i64::from(i))
    }

    pub fn emit_const_integer(&mut self, i: i64) -> cranelift_codegen::ir::Value {
        self.builder().ins().iconst(I64, i)
    }

    pub fn emit_const_bool(&mut self, b: bool) -> cranelift_codegen::ir::Value {
        self.builder().ins().iconst(I8, if b { 1 } else { 0 })
    }

    pub fn emit_const_real(&mut self, f: f64) -> cranelift_codegen::ir::Value {
        self.builder().ins().f64const(f)
    }

    pub fn emit_const(
        &mut self,
        c: pasko_frontend::constant::Constant,
    ) -> cranelift_codegen::ir::Value {
        match c {
            Constant::Integer(x) => self.emit_const_integer(x as i64),
            Constant::Real(x) => self.emit_const_real(x),
            Constant::Bool(x) => self.emit_const_bool(x),
        }
    }

    pub fn new(
        codegen: &'a mut CodegenVisitor<'b>,
        builder_obj: Option<FunctionBuilder<'c>>,
    ) -> FunctionCodegenVisitor<'a, 'b, 'c> {
        FunctionCodegenVisitor {
            codegen,
            builder_obj,

            block_stack: vec![],
            value_map: HashMap::new(),
            var_counter: 0,
            entry_block: None,
            data_references: HashMap::new(),
            function_references: HashMap::new(),
            temporaries_to_dispose: vec![],
            symbols_to_dispose: vec![],
        }
    }

    pub fn get_variable(&mut self) -> cranelift_frontend::Variable {
        let v = cranelift_frontend::Variable::new(self.var_counter);

        self.var_counter += 1;

        v
    }

    pub fn allocate_storage_in_stack(&mut self, size: u32) -> StackSlot {
        self.builder_obj
            .as_mut()
            .unwrap()
            .create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, size))
    }

    pub fn allocate_value_in_stack(&mut self, sym_id: pasko_frontend::symbol::SymbolId) {
        let symbol = self.codegen.semantic_context.get_symbol(sym_id);
        let symbol = symbol.borrow();
        let symbol_type = symbol.get_type().unwrap();

        let size = self.codegen.size_in_bytes(symbol_type) as u32;

        let stack_slot = self.allocate_storage_in_stack(size);

        self.codegen
            .data_location
            .insert(sym_id, DataLocation::StackVarValue(stack_slot));
    }

    pub fn allocate_address_in_stack(&mut self, sym_id: pasko_frontend::symbol::SymbolId) {
        let size = self.codegen.pointer_type.bytes();

        let stack_slot = self.allocate_storage_in_stack(size);

        self.codegen
            .data_location
            .insert(sym_id, DataLocation::StackVarAddress(stack_slot));
    }

    pub fn copy_in_function_parameter(
        &mut self,
        param_idx: usize,
        sym_id: pasko_frontend::symbol::SymbolId,
    ) {
        let builder = self.builder_obj.as_mut().unwrap();
        let param_value = builder.block_params(self.entry_block.unwrap())[param_idx];

        let location = self.codegen.data_location.get(&sym_id).unwrap();

        let stack_slot = match location {
            DataLocation::StackVarValue(stack_slot) | DataLocation::StackVarAddress(stack_slot) => {
                *stack_slot
            }
            _ => {
                panic!("Unexpected data location for parameter");
            }
        };
        let builder = self.builder_obj.as_mut().unwrap();

        let stack_address = builder
            .ins()
            .stack_addr(self.codegen.pointer_type, stack_slot, 0);

        builder.ins().store(
            cranelift_codegen::ir::MemFlags::new(),
            param_value,
            stack_address,
            0,
        );
    }

    pub fn load_symbol_from_stack(
        &mut self,
        sym_id: pasko_frontend::symbol::SymbolId,
    ) -> cranelift_codegen::ir::Value {
        let location = self.codegen.data_location.get(&sym_id).unwrap();

        match location {
            DataLocation::StackVarValue(stack_slot) => {
                // let param_value =
                let builder = self.builder_obj.as_mut().unwrap();

                let stack_address =
                    builder
                        .ins()
                        .stack_addr(self.codegen.pointer_type, *stack_slot, 0);

                let symbol = self.codegen.semantic_context.get_symbol(sym_id);
                let symbol = symbol.borrow();
                let ty = symbol.get_type().unwrap();

                let value = if self.codegen.semantic_context.type_system.is_simple_type(ty)
                    || self
                        .codegen
                        .semantic_context
                        .type_system
                        .is_pointer_type(ty)
                {
                    let cranelift_ty = self.codegen.type_to_cranelift_type(ty);

                    builder.ins().load(
                        cranelift_ty,
                        cranelift_codegen::ir::MemFlags::new(),
                        stack_address,
                        0,
                    )
                } else if self.codegen.semantic_context.type_system.is_array_type(ty)
                    || self.codegen.semantic_context.type_system.is_record_type(ty)
                {
                    stack_address
                } else {
                    panic!(
                        "Unexpected type {}",
                        self.codegen.semantic_context.type_system.get_type_name(ty)
                    );
                };

                value
            }
            DataLocation::StackVarAddress(stack_slot) => {
                // let param_value =
                let builder = self.builder_obj.as_mut().unwrap();

                let stack_address =
                    builder
                        .ins()
                        .stack_addr(self.codegen.pointer_type, *stack_slot, 0);

                // Load the address in the stack.
                let variable_address = builder.ins().load(
                    self.codegen.pointer_type,
                    cranelift_codegen::ir::MemFlags::new(),
                    stack_address,
                    0,
                );

                let symbol = self.codegen.semantic_context.get_symbol(sym_id);
                let symbol = symbol.borrow();
                let ty = symbol.get_type().unwrap();

                let cranelift_ty = self.codegen.type_to_cranelift_type(ty);
                let value = builder.ins().load(
                    cranelift_ty,
                    cranelift_codegen::ir::MemFlags::new(),
                    variable_address,
                    0,
                );

                value
            }
            _ => {
                panic!("Unexpected data location for parameter");
            }
        }
    }

    pub fn get_global_value(
        &mut self,
        data_id: cranelift_module::DataId,
    ) -> cranelift_codegen::ir::GlobalValue {
        let gv = match self.data_references.entry(data_id) {
            std::collections::hash_map::Entry::Occupied(entry) => *entry.get(),
            std::collections::hash_map::Entry::Vacant(entry) => {
                let object_module = self.codegen.object_module.as_ref().unwrap();
                let func = &mut self.builder_obj.as_mut().unwrap().func;
                let gv = object_module.declare_data_in_func(data_id, *func);

                entry.insert(gv);
                gv
            }
        };

        gv
    }

    pub fn get_function_reference(
        &mut self,
        func_id: cranelift_module::FuncId,
    ) -> cranelift_codegen::ir::FuncRef {
        let func_ref = match self.function_references.entry(func_id) {
            std::collections::hash_map::Entry::Occupied(entry) => *entry.get(),
            std::collections::hash_map::Entry::Vacant(entry) => {
                let func = &mut self.builder_obj.as_mut().unwrap().func;
                let func_ref = self
                    .codegen
                    .object_module
                    .as_mut()
                    .unwrap()
                    .declare_func_in_func(func_id, *func);

                entry.insert(func_ref);
                func_ref
            }
        };

        func_ref
    }

    fn call_pass_arguments(
        &mut self,
        args: Vec<(&span::SpannedBox<ast::Expr>, ParameterKind)>,
    ) -> Vec<cranelift_codegen::ir::Value> {
        args.iter()
            .map(|item| {
                let (arg, param_kind) = item;
                let arg_ty = self
                    .codegen
                    .semantic_context
                    .get_ast_type(arg.id())
                    .unwrap();
                let arg_value = *self.value_map.get(&arg.id()).unwrap();
                if *param_kind == ParameterKind::Variable
                    || self
                        .codegen
                        .semantic_context
                        .type_system
                        .is_simple_type(arg_ty)
                    || self
                        .codegen
                        .semantic_context
                        .type_system
                        .is_pointer_type(arg_ty)
                {
                    arg_value
                } else if self
                    .codegen
                    .semantic_context
                    .type_system
                    .is_array_type(arg_ty)
                    || self
                        .codegen
                        .semantic_context
                        .type_system
                        .is_record_type(arg_ty)
                {
                    if !self.codegen.is_trivially_copyable(arg_ty) {
                        unimplemented!("Non-trivially copyable types");
                    }
                    // We need to do a copy in.
                    let arg_type_size = self.codegen.size_in_bytes(arg_ty);
                    let stack_slot = self.allocate_storage_in_stack(arg_type_size as u32);
                    let pointer = self.codegen.pointer_type;
                    let stack_addr = self.builder().ins().stack_addr(pointer, stack_slot, 0);
                    // Now copy
                    let target_config = {
                        let object_module = self.codegen.object_module.as_ref().unwrap();
                        object_module.target_config()
                    };
                    let arg_type_size_value = self.emit_const_integer(arg_type_size as i64);
                    self.builder().call_memcpy(
                        target_config,
                        stack_addr,
                        arg_value,
                        arg_type_size_value,
                    );
                    stack_addr
                } else if self
                    .codegen
                    .semantic_context
                    .type_system
                    .is_set_type(arg_ty)
                {
                    match arg.get() {
                        ast::Expr::Variable(..) | ast::Expr::VariableReference(..) => {
                            // We need to copy the pointer from this variable.
                            let func_id = self.codegen.rt.set_copy.unwrap();
                            let func_ref = self.get_function_reference(func_id);
                            let call = self.builder().ins().call(func_ref, &[arg_value]);

                            let result = {
                                let results = self.builder().inst_results(call);
                                assert!(results.len() == 1, "Invalid number of results");
                                results[0]
                            };

                            // This temporary is disposed by the callee.
                            result
                        }
                        _ => {
                            // We don't have to copy, just pass the temporary.
                            // This temporary will be disposed by the callee.
                            self.remove_temporary_to_dispose(arg_value);
                            arg_value
                        }
                    }
                } else {
                    panic!(
                        "Unexpected type in argument pass {}",
                        self.codegen
                            .semantic_context
                            .type_system
                            .get_type_name(arg_ty)
                    );
                }
            })
            .collect()
    }

    fn new_temporary_to_dispose(
        &mut self,
        addr: cranelift_codegen::ir::Value,
        ty: pasko_frontend::typesystem::TypeId,
    ) {
        self.temporaries_to_dispose.push(Temporary { addr, ty });
    }

    fn remove_temporary_to_dispose(&mut self, addr: cranelift_codegen::ir::Value) {
        self.temporaries_to_dispose.retain(|x| x.addr != addr);
    }

    fn dispose_set_variable(&mut self, addr: cranelift_codegen::ir::Value) {
        let func_id = self.codegen.rt.set_dispose.unwrap();
        let func_ref = self.get_function_reference(func_id);
        self.builder().ins().call(func_ref, &[addr]);
    }

    fn dispose_temporaries(&mut self) {
        let temporaries = std::mem::take(&mut self.temporaries_to_dispose);

        for Temporary { addr, ty } in temporaries {
            if self.codegen.semantic_context.type_system.is_set_type(ty) {
                self.dispose_set_variable(addr);
            } else {
                panic!(
                    "Unexpected temporary of type {} to dispose",
                    self.codegen.semantic_context.type_system.get_type_name(ty)
                );
            }
        }
    }

    fn get_address_of_data_location(
        &mut self,
        storage: DataLocation,
    ) -> cranelift_codegen::ir::Value {
        match storage {
            DataLocation::GlobalVar(data_id) => {
                // Compute the address.
                let gv = self.get_global_value(data_id);
                let addr = self
                    .builder_obj
                    .as_mut()
                    .unwrap()
                    .ins()
                    .global_value(self.codegen.pointer_type, gv);

                addr
            }
            DataLocation::StackVarValue(stack_slot) => {
                let pointer = self.codegen.pointer_type;
                let addr = self.builder().ins().stack_addr(pointer, stack_slot, 0);

                addr
            }
            DataLocation::StackVarAddress(stack_slot) => {
                let pointer_type = self.codegen.pointer_type;
                let stack_addr = self.builder().ins().stack_addr(pointer_type, stack_slot, 0);

                let addr = self.builder().ins().load(
                    pointer_type,
                    cranelift_codegen::ir::MemFlags::new(),
                    stack_addr,
                    0,
                );

                addr
            }
        }
    }

    pub fn add_symbol_to_dispose(&mut self, sym: pasko_frontend::symbol::SymbolId) {
        debug_assert!(
            self.symbols_to_dispose.iter().find(|&&s| s == sym) == None,
            "adding twice a symbol for disposal"
        );
        self.symbols_to_dispose.push(sym);
    }

    pub fn dispose_symbols(&mut self) {
        let symbols_to_dispose = std::mem::take(&mut self.symbols_to_dispose);

        for sym_id in symbols_to_dispose {
            let sym = self.codegen.semantic_context.get_symbol(sym_id);
            let sym = sym.borrow();
            let ty = sym.get_type().unwrap();

            if self.codegen.semantic_context.type_system.is_set_type(ty) {
                let data_loc = self.codegen.data_location.get(&sym_id).unwrap().clone();
                let addr = match data_loc {
                    DataLocation::GlobalVar(..) | DataLocation::StackVarValue(..) => {
                        self.get_address_of_data_location(data_loc)
                    }
                    _ => panic!("Unexpected location"),
                };
                let pointer_type = self.codegen.pointer_type;
                let pointer_value = self.builder().ins().load(
                    pointer_type,
                    cranelift_codegen::ir::MemFlags::new(),
                    addr,
                    0,
                );
                self.dispose_set_variable(pointer_value);
            } else {
                panic!(
                    "Symbol {} has unexpected type {} to dispose",
                    sym.get_name(),
                    self.codegen.semantic_context.type_system.get_type_name(ty)
                );
            }
        }
    }
}

impl<'a, 'b, 'c> VisitorMut for FunctionCodegenVisitor<'a, 'b, 'c> {
    fn visit_pre_stmt_compound(
        &mut self,
        n: &ast::StmtCompound,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        n.0.iter()
            .for_each(|c| c.get().walk_mut(self, c.loc(), c.id()));
        false
    }

    fn visit_pre_stmt_procedure_call(
        &mut self,
        n: &ast::StmtProcedureCall,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        // Only writeln is supported atm

        let procedure_name = n.0.get().as_str();

        if pasko_frontend::semantic::is_required_procedure(procedure_name) {
            match procedure_name {
                "writeln" => {
                    if let Some(args) = &n.1 {
                        let zero = self.builder().ins().iconst(I32, 0);
                        for arg in args {
                            let (v, type_id, total_width, fract_digits): (
                                cranelift_codegen::ir::Value,
                                TypeId,
                                cranelift_codegen::ir::Value,
                                cranelift_codegen::ir::Value,
                            ) = match arg.get() {
                                ast::Expr::WriteParameter(_) => {
                                    panic!("Write parameter not implemented yet!");
                                }
                                _ => {
                                    arg.get().walk_mut(self, arg.loc(), arg.id());
                                    (
                                        self.get_value(arg.id()),
                                        self.codegen
                                            .semantic_context
                                            .get_ast_type(arg.id())
                                            .unwrap(),
                                        zero,
                                        zero,
                                    )
                                }
                            };

                            if self
                                .codegen
                                .semantic_context
                                .type_system
                                .is_integer_type(type_id)
                            {
                                let func_id = self.codegen.rt.write_i64.unwrap();
                                let func_ref = self.get_function_reference(func_id);
                                self.builder().ins().call(func_ref, &[v, total_width]);
                            } else if self
                                .codegen
                                .semantic_context
                                .type_system
                                .is_real_type(type_id)
                            {
                                let func_id = self.codegen.rt.write_f64.unwrap();
                                let func_ref = self.get_function_reference(func_id);
                                self.builder()
                                    .ins()
                                    .call(func_ref, &[v, total_width, fract_digits]);
                            } else if self
                                .codegen
                                .semantic_context
                                .type_system
                                .is_string_type(type_id)
                            {
                                let func_id = self.codegen.rt.write_str.unwrap();
                                let func_ref = self.get_function_reference(func_id);
                                self.builder().ins().call(func_ref, &[v]);
                            } else if self
                                .codegen
                                .semantic_context
                                .type_system
                                .is_bool_type(type_id)
                            {
                                let func_id = self.codegen.rt.write_bool.unwrap();
                                let func_ref = self.get_function_reference(func_id);
                                self.builder().ins().call(func_ref, &[v]);
                            } else if self
                                .codegen
                                .semantic_context
                                .type_system
                                .is_char_type(type_id)
                            {
                                let func_id = self.codegen.rt.write_char.unwrap();
                                let func_ref = self.get_function_reference(func_id);
                                self.builder().ins().call(func_ref, &[v]);
                            } else {
                                panic!(
                                    "Unexpected type for writeln {}",
                                    self.codegen
                                        .semantic_context
                                        .type_system
                                        .get_type_name(type_id)
                                );
                            }
                        }
                    }

                    {
                        let func_id = self.codegen.rt.write_newline.unwrap();
                        let func_ref = self.get_function_reference(func_id);
                        self.builder().ins().call(func_ref, &[]);
                    }
                }
                "readln" => {
                    if let Some(args) = &n.1 {
                        for arg in args {
                            match arg.get() {
                                ast::Expr::Variable(expr_var) => {
                                    let var = &expr_var.0;

                                    var.get().walk_mut(self, var.loc(), var.id());
                                    let var_addr = self.get_value(var.id());

                                    let var_ty = self
                                        .codegen
                                        .semantic_context
                                        .get_ast_type(var.id())
                                        .unwrap();

                                    if self
                                        .codegen
                                        .semantic_context
                                        .type_system
                                        .is_integer_type(var_ty)
                                        || self
                                            .codegen
                                            .semantic_context
                                            .type_system
                                            .is_real_type(var_ty)
                                    {
                                        let func_id = if self
                                            .codegen
                                            .semantic_context
                                            .type_system
                                            .is_integer_type(var_ty)
                                        {
                                            self.codegen.rt.read_i64.unwrap()
                                        } else {
                                            self.codegen.rt.read_f64.unwrap()
                                        };
                                        let func_ref = self.get_function_reference(func_id);

                                        let call = self.builder().ins().call(func_ref, &[]);
                                        let result = {
                                            let results = self.builder().inst_results(call);
                                            assert!(
                                                results.len() == 1,
                                                "Invalid number of results"
                                            );
                                            results[0]
                                        };

                                        self.builder().ins().store(
                                            cranelift_codegen::ir::MemFlags::new(),
                                            result,
                                            var_addr,
                                            0,
                                        );
                                    } else {
                                        panic!(
                                            "Unexpected type for readln {}",
                                            self.codegen
                                                .semantic_context
                                                .type_system
                                                .get_type_name(var_ty)
                                        );
                                    }
                                }
                                _ => {
                                    panic!("Invalid AST at this point");
                                }
                            }
                        }
                    }
                    {
                        let func_id = self.codegen.rt.read_newline.unwrap();
                        let func_ref = self.get_function_reference(func_id);
                        self.builder().ins().call(func_ref, &[]);
                    }
                }
                "new" => {
                    if let Some(args) = &n.1 {
                        assert!(args.len() == 1);
                        let arg = &args[0];
                        let (var_addr, var_ty) = match arg.get() {
                            ast::Expr::Variable(expr_var) => {
                                let var = &expr_var.0;

                                var.get().walk_mut(self, var.loc(), var.id());
                                let pointer_ty = self
                                    .codegen
                                    .semantic_context
                                    .get_ast_type(var.id())
                                    .unwrap();

                                let pointee_ty = self
                                    .codegen
                                    .semantic_context
                                    .type_system
                                    .pointer_type_get_pointee_type(pointer_ty);

                                (self.get_value(var.id()), pointee_ty)
                            }
                            _ => {
                                panic!("Invalid AST at this point!");
                            }
                        };

                        let size_bytes = self.codegen.size_in_bytes(var_ty) as i64;
                        let size_bytes = self.emit_const_integer(size_bytes);

                        let func_id = self.codegen.rt.pointer_new.unwrap();
                        let func_ref = self.get_function_reference(func_id);
                        self.builder().ins().call(func_ref, &[var_addr, size_bytes]);
                    }
                }
                "dispose" => {
                    if let Some(args) = &n.1 {
                        assert!(args.len() == 1);
                        let arg = &args[0];
                        let var_addr = match arg.get() {
                            ast::Expr::Variable(expr_var) => {
                                let var = &expr_var.0;

                                var.get().walk_mut(self, var.loc(), var.id());
                                self.get_value(var.id())
                            }
                            _ => {
                                panic!("Invalid AST at this point!");
                            }
                        };

                        let func_id = self.codegen.rt.pointer_dispose.unwrap();
                        let func_ref = self.get_function_reference(func_id);
                        self.builder().ins().call(func_ref, &[var_addr]);
                    }
                }
                _ => {
                    panic!(
                        "Lowering of call to required procedure {} not implemented yet",
                        procedure_name
                    );
                }
            }
        } else {
            let args = &n.1;

            if let Some(args) = args {
                args.iter().for_each(|arg| {
                    arg.get().walk_mut(self, arg.loc(), arg.id());
                });
            }

            let callee = &n.0;
            let callee_sym_id = self
                .codegen
                .semantic_context
                .get_ast_symbol(callee.id())
                .unwrap();
            let callee_sym = self.codegen.semantic_context.get_symbol(callee_sym_id);
            let callee_sym = callee_sym.borrow();
            let callee_parameters = callee_sym.get_formal_parameters().unwrap();

            let func_id = *self
                .codegen
                .function_identifiers
                .get(&callee_sym_id)
                .unwrap();

            let func_ref = self.get_function_reference(func_id);

            let arg_values = if let Some(args) = args {
                assert!(args.len() == callee_parameters.len());
                let args = args
                    .iter()
                    .zip(callee_parameters)
                    .map(|(value, param)| {
                        let param_symbol = self.codegen.semantic_context.get_symbol(param);
                        let param_symbol = param_symbol.borrow();
                        (value, param_symbol.get_parameter().unwrap())
                    })
                    .collect::<Vec<_>>();
                self.call_pass_arguments(args)
            } else {
                vec![]
            };

            self.builder().ins().call(func_ref, arg_values.as_slice());
        }

        // Dispose temporaries created during parameter passing.
        self.dispose_temporaries();

        false
    }

    fn visit_pre_expr_const(
        &mut self,
        n: &ast::ExprConst,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        n.0.get().walk_mut(self, n.0.loc(), n.0.id());
        self.set_value(id, self.get_value(n.0.id()));
        false
    }

    fn visit_const_integer(
        &mut self,
        n: &ast::ConstInteger,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let v = self.emit_const_integer(*n.0.get() as i64);
        self.set_value(id, v);
    }

    fn visit_const_real(&mut self, n: &ast::ConstReal, _span: &span::SpanLoc, id: span::SpanId) {
        let v = self.emit_const_real(*n.0.get());
        self.set_value(id, v);
    }

    fn visit_const_string_literal(
        &mut self,
        n: &ast::ConstStringLiteral,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let literal_ty = self.codegen.semantic_context.get_ast_type(id).unwrap();

        if self
            .codegen
            .semantic_context
            .type_system
            .is_char_type(literal_ty)
        {
            let x = n.0.get().chars().collect::<Vec<_>>();
            assert!(x.len() == 1);
            let x = u32::from(x[0]);

            let v = self.emit_const_char(x);
            self.set_value(id, v);
        } else if self
            .codegen
            .semantic_context
            .type_system
            .is_string_type(literal_ty)
        {
            let data_id = match self.codegen.string_table.entry(n.0.get().to_owned()) {
                std::collections::hash_map::Entry::Occupied(entry) => *entry.get(),
                std::collections::hash_map::Entry::Vacant(entry) => {
                    let data_id = self
                        .codegen
                        .object_module
                        .as_mut()
                        .unwrap()
                        .declare_anonymous_data(false, false)
                        .unwrap();
                    entry.insert(data_id);

                    let mut data_desc = cranelift_module::DataDescription::new();
                    let mut unicode_points =
                        n.0.get().chars().map(|x| u32::from(x)).collect::<Vec<_>>();
                    unicode_points.push(0); // NULL.
                    let bytes = unicode_points
                        .iter()
                        // FIXME: endianness is dependent of the platform
                        .map(|x| x.to_le_bytes())
                        .flatten()
                        .collect::<Vec<_>>();
                    data_desc.define(bytes.into_boxed_slice());

                    self.codegen
                        .object_module
                        .as_mut()
                        .unwrap()
                        .define_data(data_id, &data_desc)
                        .unwrap();

                    data_id
                }
            };

            let pointer_type = self.codegen.pointer_type;
            let gv = self.get_global_value(data_id);

            // This is the address???
            let v = self.builder().ins().global_value(pointer_type, gv);
            self.set_value(id, v);
        } else {
            panic!(
                "Unexpected type {} for string literal",
                self.codegen
                    .semantic_context
                    .type_system
                    .get_type_name(literal_ty)
            );
        }
    }

    fn visit_const_nil(&mut self, _n: &ast::ConstNil, _span: &span::SpanLoc, id: span::SpanId) {
        let pointer_type = self.codegen.pointer_type;
        let v = self.builder().ins().iconst(pointer_type, 0);
        self.set_value(id, v);
    }

    fn visit_pre_expr_set_literal(
        &mut self,
        n: &ast::ExprSetLiteral,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        n.0.iter().for_each(|e| {
            e.get().walk_mut(self, e.loc(), e.id());
        });

        let set_type = self.codegen.semantic_context.get_ast_type(id).unwrap();
        let element_type = self
            .codegen
            .semantic_context
            .type_system
            .set_type_get_element(set_type);

        let number_of_elements = n.0.len();
        let call = if number_of_elements > 0 {
            let element_size = self.codegen.size_in_bytes(element_type);
            let stack_slot =
                self.allocate_storage_in_stack((element_size * number_of_elements) as u32);
            let pointer = self.codegen.pointer_type;
            let stack_addr = self.builder().ins().stack_addr(pointer, stack_slot, 0);
            for (index, value) in n.0.iter().enumerate() {
                let value = self.get_value(value.id());
                self.builder().ins().store(
                    cranelift_codegen::ir::MemFlags::new(),
                    value,
                    stack_addr,
                    (element_size * index) as i32,
                );
            }

            let number_of_elements_value = self.emit_const_integer(number_of_elements as i64);

            let func_id = self.codegen.rt.set_new.unwrap();
            let func_ref = self.get_function_reference(func_id);
            self.builder()
                .ins()
                .call(func_ref, &[number_of_elements_value, stack_addr])
        } else {
            let zero = self.emit_const_integer(0 as i64);
            let func_id = self.codegen.rt.set_new.unwrap();
            let func_ref = self.get_function_reference(func_id);
            self.builder().ins().call(func_ref, &[zero, zero])
        };

        let result = {
            let results = self.builder().inst_results(call);
            assert!(results.len() == 1, "Invalid number of results");
            results[0]
        };

        self.set_value(id, result);

        false
    }

    fn visit_const_named(&mut self, _n: &ast::ConstNamed, _span: &span::SpanLoc, id: span::SpanId) {
        let v = self.codegen.semantic_context.get_ast_value(id).unwrap();
        let value = self.emit_const(v);
        self.set_value(id, value);
    }

    fn visit_pre_stmt_assignment(
        &mut self,
        n: &ast::StmtAssignment,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        n.1.get().walk_mut(self, n.1.loc(), n.1.id());
        let value = self.get_value(n.1.id());

        n.0.get().walk_mut(self, n.0.loc(), n.0.id());
        let addr = self.get_value(n.0.id());
        let ty = self
            .codegen
            .semantic_context
            .get_ast_type(n.0.id())
            .unwrap();

        if self.codegen.semantic_context.type_system.is_simple_type(ty) {
            self.builder()
                .ins()
                .store(cranelift_codegen::ir::MemFlags::new(), value, addr, 0);
        } else if self.codegen.semantic_context.type_system.is_array_type(ty)
            || self.codegen.semantic_context.type_system.is_record_type(ty)
        {
            let rhs_ty = self
                .codegen
                .semantic_context
                .get_ast_type(n.1.id())
                .unwrap();
            assert!(
                self.codegen.size_in_bytes(ty) == self.codegen.size_in_bytes(rhs_ty),
                "Type sizes must match"
            );
            let size = self.emit_const_integer(self.codegen.size_in_bytes(ty) as i64);
            let target_config = {
                let object_module = self.codegen.object_module.as_ref().unwrap();
                object_module.target_config()
            };
            self.builder()
                .call_memmove(target_config, addr, value, size);
        } else if self.codegen.semantic_context.type_system.is_set_type(ty) {
            // To assign we need first to free the lhs and then copy, if the right hand side is a temporary we can just move the value.
            let pointer_type = self.codegen.pointer_type;
            let pointer_value = self.builder().ins().load(
                pointer_type,
                cranelift_codegen::ir::MemFlags::new(),
                addr,
                0,
            );
            self.dispose_set_variable(pointer_value);

            let src_value = {
                match n.1.get() {
                    ast::Expr::Variable(..) | ast::Expr::VariableReference(..) => {
                        // We need to copy the pointer from this variable.
                        let func_id = self.codegen.rt.set_copy.unwrap();
                        let func_ref = self.get_function_reference(func_id);
                        let call = self.builder().ins().call(func_ref, &[value]);

                        let result = {
                            let results = self.builder().inst_results(call);
                            assert!(results.len() == 1, "Invalid number of results");
                            results[0]
                        };

                        result
                    }
                    _ => {
                        // This temporary does not have to disposed after this expression anymore.
                        self.remove_temporary_to_dispose(value);
                        value
                    }
                }
            };

            // Finally, store the address of the rhs set.
            self.builder()
                .ins()
                .store(cranelift_codegen::ir::MemFlags::new(), src_value, addr, 0);
        } else if self
            .codegen
            .semantic_context
            .type_system
            .is_pointer_type(ty)
        {
            self.builder()
                .ins()
                .store(cranelift_codegen::ir::MemFlags::new(), value, addr, 0);
        } else {
            panic!(
                "Do not know how to assign a value of type {}",
                self.codegen.semantic_context.type_system.get_type_name(ty)
            );
        }

        // Dispose the temporaries we may have created while evaluating the rhs.
        self.dispose_temporaries();

        false
    }

    fn visit_pre_expr_variable(
        &mut self,
        n: &ast::ExprVariable,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        n.0.get().walk_mut(self, n.0.loc(), n.0.id());

        let addr = self.get_value(n.0.id());
        let ty = self
            .codegen
            .semantic_context
            .get_ast_type(n.0.id())
            .unwrap();

        if self.codegen.semantic_context.type_system.is_simple_type(ty) {
            let cranelift_ty = self.codegen.type_to_cranelift_type(ty);

            let v = self.builder().ins().load(
                cranelift_ty,
                cranelift_codegen::ir::MemFlags::new(),
                addr,
                0,
            );

            self.set_value(id, v);
        } else if self.codegen.semantic_context.type_system.is_array_type(ty)
            || self.codegen.semantic_context.type_system.is_record_type(ty)
        {
            // Structured types cannot have value semantics in the cranelift IR.
            self.set_value(id, addr);
        } else if self.codegen.semantic_context.type_system.is_set_type(ty) {
            // Set types are opaque pointers so in some sense they're like simple types
            // but with an opaque pointer type.
            let pointer_type = self.codegen.pointer_type;
            let v = self.builder().ins().load(
                pointer_type,
                cranelift_codegen::ir::MemFlags::new(),
                addr,
                0,
            );

            self.set_value(id, v);
        } else if self
            .codegen
            .semantic_context
            .type_system
            .is_pointer_type(ty)
        {
            let pointer_type = self.codegen.pointer_type;
            let v = self.builder().ins().load(
                pointer_type,
                cranelift_codegen::ir::MemFlags::new(),
                addr,
                0,
            );
            self.set_value(id, v);
        } else {
            panic!(
                "Unhandled type {} in variable reference",
                self.codegen.semantic_context.type_system.get_type_name(ty)
            );
        }

        false
    }

    fn visit_pre_expr_variable_reference(
        &mut self,
        n: &ast::ExprVariableReference,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        n.0.get().walk_mut(self, n.0.loc(), n.0.id());

        let addr = self.get_value(n.0.id());

        self.set_value(id, addr);

        false
    }

    fn visit_assig_variable(
        &mut self,
        _n: &ast::AssigVariable,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        //
        let sym_id = self.codegen.semantic_context.get_ast_symbol(id).unwrap();
        let sym = self.codegen.semantic_context.get_symbol(sym_id);
        let sym = sym.borrow();

        let addr_value = match sym.get_kind() {
            symbol::SymbolKind::Variable => {
                let storage = *self.codegen.data_location.get(&sym_id).unwrap();
                self.get_address_of_data_location(storage)
            }
            _ => {
                panic!(
                    "Unexpected kind of symbol {}",
                    sym.get_name_of_kind().unwrap_or("<unknown symbol name???>")
                );
            }
        };

        self.set_value(id, addr_value);
    }

    fn visit_pre_assig_array_access(
        &mut self,
        n: &ast::AssigArrayAccess,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        // Walk the left hand side.
        n.0.get().walk_mut(self, n.0.loc(), n.0.id());
        let array_addr = self.get_value(n.0.id());
        let array_ty = self
            .codegen
            .semantic_context
            .get_ast_type(n.0.id())
            .unwrap();

        let num_idx = n.1.len();
        let (_final_type, index_type_sizes, index_type_lower_bounds) = n.1.iter().enumerate().fold(
            (array_ty, Vec::new(), Vec::new()),
            |acc, (current_idx, ..)| {
                let current_array_type = acc.0;

                let current_array_index_type = self
                    .codegen
                    .semantic_context
                    .type_system
                    .array_type_get_index_type(current_array_type);
                let lower_bound = self
                    .codegen
                    .semantic_context
                    .type_system
                    .ordinal_type_lower_bound(current_array_index_type);

                let current_array_component_type = self
                    .codegen
                    .semantic_context
                    .type_system
                    .array_type_get_component_type(current_array_type);

                let (index_type_size, index_type_lb) = if current_idx + 1 == num_idx {
                    (
                        self.codegen.size_in_bytes(current_array_component_type) as i64,
                        lower_bound,
                    )
                } else {
                    if self
                        .codegen
                        .semantic_context
                        .type_system
                        .is_array_type(current_array_component_type)
                    {
                        let index_type = self
                            .codegen
                            .semantic_context
                            .type_system
                            .array_type_get_index_type(current_array_component_type);
                        let extent = self
                            .codegen
                            .semantic_context
                            .type_system
                            .ordinal_type_extent(index_type);
                        (extent, lower_bound)
                    } else {
                        panic!("Expecting an array type");
                    }
                };

                let mut index_type_sizes = acc.1;
                index_type_sizes.push(index_type_size);

                let mut index_type_lbs = acc.2;
                index_type_lbs.push(index_type_lb);

                (
                    current_array_component_type,
                    index_type_sizes,
                    index_type_lbs,
                )
            },
        );

        let mut linear_index_bytes = None;
        for ((index, index_size), lb) in
            n.1.iter()
                .zip(index_type_sizes)
                .zip(index_type_lower_bounds)
        {
            index.get().walk_mut(self, index.loc(), index.id());
            let index_value = self.get_value(index.id());

            let lb_value = self.emit_const_integer(lb);
            let offset = self.builder().ins().isub(index_value, lb_value);
            let offset = if let Some(linear_index_bytes) = linear_index_bytes {
                self.builder().ins().iadd(index_value, linear_index_bytes)
            } else {
                offset
            };

            let index_size_value = self.emit_const_integer(index_size);
            linear_index_bytes = Some(self.builder().ins().imul(offset, index_size_value));
        }

        let linear_index_bytes = linear_index_bytes.unwrap();

        let addr = self.builder().ins().iadd(array_addr, linear_index_bytes);

        self.set_value(id, addr);

        false
    }

    fn visit_pre_assig_field_access(
        &mut self,
        n: &ast::AssigFieldAccess,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        n.0.get().walk_mut(self, n.0.loc(), n.0.id());
        let record_addr = self.get_value(n.0.id());

        // Make sure the type has been laid out.
        let _ = self.codegen.size_in_bytes(
            self.codegen
                .semantic_context
                .get_ast_type(n.0.id())
                .unwrap(),
        );

        let field_sym = self
            .codegen
            .semantic_context
            .get_ast_symbol(n.1.id())
            .unwrap();

        let offset = {
            let cache = self.codegen.offset_cache.borrow();
            *cache.get(&field_sym).unwrap()
        };
        let offset = self.emit_const_integer(offset as i64);

        let addr = self.builder().ins().iadd(record_addr, offset);
        self.set_value(id, addr);

        false
    }

    fn visit_pre_assig_pointer_deref(
        &mut self,
        n: &ast::AssigPointerDeref,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        n.0.get().walk_mut(self, n.0.loc(), n.0.id());
        let pointer_addr = self.get_value(n.0.id());

        let pointer_ty = self.codegen.pointer_type;
        let pointer_value = self.builder().ins().load(
            pointer_ty,
            cranelift_codegen::ir::MemFlags::new(),
            pointer_addr,
            0,
        );
        self.set_value(id, pointer_value);

        false
    }

    fn visit_pre_expr_parentheses(
        &mut self,
        n: &ast::ExprParentheses,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        n.0.get().walk_mut(self, n.0.loc(), n.0.id());

        self.set_value(id, self.get_value(n.0.id()));

        false
    }

    fn visit_pre_expr_un_op(
        &mut self,
        n: &ast::ExprUnOp,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        n.1.get().walk_mut(self, n.1.loc(), n.1.id());

        let operator = n.0.get();
        let op_value = self.get_value(n.1.id());

        match operator {
            UnaryOp::LogicalNot => {
                let result = self.builder().ins().irsub_imm(op_value, 1);
                self.set_value(id, result);
            }
            UnaryOp::Negation => {
                let result = self.builder().ins().ineg(op_value);
                self.set_value(id, result);
            }
            UnaryOp::Plus => {
                self.set_value(id, op_value);
            }
        }

        false
    }

    fn visit_pre_expr_bin_op(
        &mut self,
        n: &ast::ExprBinOp,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        n.1.get().walk_mut(self, n.1.loc(), n.1.id());
        n.2.get().walk_mut(self, n.2.loc(), n.2.id());

        let operator = n.0.get();
        let lhs_value = self.get_value(n.1.id());
        let rhs_value = self.get_value(n.2.id());
        let op_type = self.codegen.semantic_context.get_ast_type(id).unwrap();
        let is_integer = self
            .codegen
            .semantic_context
            .type_system
            .is_integer_type(op_type);
        let is_real = self
            .codegen
            .semantic_context
            .type_system
            .is_real_type(op_type);
        let is_bool = self
            .codegen
            .semantic_context
            .type_system
            .is_bool_type(op_type);
        let is_set = self
            .codegen
            .semantic_context
            .type_system
            .is_set_type(op_type);
        if is_integer {
            match operator {
                ast::BinOperand::Addition => {
                    let result = self.builder().ins().iadd(lhs_value, rhs_value);
                    self.set_value(id, result);
                }
                ast::BinOperand::Subtraction => {
                    let result = self.builder().ins().isub(lhs_value, rhs_value);
                    self.set_value(id, result);
                }
                ast::BinOperand::Multiplication => {
                    let result = self.builder().ins().imul(lhs_value, rhs_value);
                    self.set_value(id, result);
                }
                ast::BinOperand::IntegerDivision => {
                    // FIXME: Check that this fulfills the spec
                    //   Cranelift: a := sign(xy) floor(abs(x) / abs(y))
                    //   Pascal: abs(i) - abs(j) < abs((i div j) * j) <= abs(i)
                    let result = self.builder().ins().sdiv(lhs_value, rhs_value);
                    self.set_value(id, result);
                }
                ast::BinOperand::Modulo => {
                    // TODO: Pascal requires rhs > 0, do we want to check this at runtime?
                    let result = self.builder().ins().srem(lhs_value, rhs_value);
                    self.set_value(id, result);
                }
                _ => {
                    panic!(
                        "Unexpected operator {} for type {}",
                        operator,
                        self.codegen
                            .semantic_context
                            .type_system
                            .get_type_name(op_type)
                    );
                }
            }
        } else if is_real {
            match operator {
                ast::BinOperand::Addition => {
                    let result = self.builder().ins().fadd(lhs_value, rhs_value);
                    self.set_value(id, result);
                }
                ast::BinOperand::Subtraction => {
                    let result = self.builder().ins().fsub(lhs_value, rhs_value);
                    self.set_value(id, result);
                }
                ast::BinOperand::Multiplication => {
                    let result = self.builder().ins().fmul(lhs_value, rhs_value);
                    self.set_value(id, result);
                }
                ast::BinOperand::RealDivision => {
                    let result = self.builder().ins().fdiv(lhs_value, rhs_value);
                    self.set_value(id, result);
                }
                _ => {
                    panic!(
                        "Unexpected operator {} for type {}",
                        operator,
                        self.codegen
                            .semantic_context
                            .type_system
                            .get_type_name(op_type)
                    );
                }
            }
        } else if is_bool {
            match operator {
                ast::BinOperand::Equal
                | ast::BinOperand::Different
                | ast::BinOperand::LowerThan
                | ast::BinOperand::LowerOrEqualThan
                | ast::BinOperand::GreaterThan
                | ast::BinOperand::GreaterOrEqualThan => {
                    let lhs_type = self
                        .codegen
                        .semantic_context
                        .get_ast_type(n.1.id())
                        .unwrap();
                    let rhs_type = self
                        .codegen
                        .semantic_context
                        .get_ast_type(n.2.id())
                        .unwrap();
                    assert!(lhs_type == rhs_type, "Invalid types in comparison");

                    if self
                        .codegen
                        .semantic_context
                        .type_system
                        .is_integer_type(lhs_type)
                        || self
                            .codegen
                            .semantic_context
                            .type_system
                            .is_bool_type(lhs_type)
                        || self
                            .codegen
                            .semantic_context
                            .type_system
                            .is_pointer_type(lhs_type)
                    {
                        let cond = match operator {
                            ast::BinOperand::Equal => IntCC::Equal,
                            ast::BinOperand::Different => IntCC::NotEqual,
                            ast::BinOperand::LowerThan => IntCC::SignedLessThan,
                            ast::BinOperand::LowerOrEqualThan => IntCC::SignedLessThanOrEqual,
                            ast::BinOperand::GreaterThan => IntCC::SignedGreaterThan,
                            ast::BinOperand::GreaterOrEqualThan => IntCC::SignedGreaterThanOrEqual,
                            _ => panic!("Unexpected operator"),
                        };
                        let result = self.builder().ins().icmp(cond, lhs_value, rhs_value);
                        self.set_value(id, result);
                    } else if self
                        .codegen
                        .semantic_context
                        .type_system
                        .is_real_type(rhs_type)
                    {
                        let cond = match operator {
                            ast::BinOperand::Equal => FloatCC::Equal,
                            ast::BinOperand::Different => FloatCC::NotEqual,
                            ast::BinOperand::LowerThan => FloatCC::LessThan,
                            ast::BinOperand::LowerOrEqualThan => FloatCC::LessThanOrEqual,
                            ast::BinOperand::GreaterThan => FloatCC::GreaterThan,
                            ast::BinOperand::GreaterOrEqualThan => FloatCC::GreaterThanOrEqual,
                            _ => panic!("Unexpected operator"),
                        };
                        let result = self.builder().ins().fcmp(cond, lhs_value, rhs_value);
                        self.set_value(id, result);
                    } else if self
                        .codegen
                        .semantic_context
                        .type_system
                        .is_set_type(rhs_type)
                    {
                        let (mut op1_value, mut op2_value) = (lhs_value, rhs_value);
                        let func_id = match operator {
                            ast::BinOperand::Equal => self.codegen.rt.set_equal.unwrap(),
                            ast::BinOperand::Different => self.codegen.rt.set_not_equal.unwrap(),
                            ast::BinOperand::LowerOrEqualThan => {
                                self.codegen.rt.set_is_subset.unwrap()
                            }
                            ast::BinOperand::GreaterOrEqualThan => {
                                // Swap operands
                                std::mem::swap(&mut op1_value, &mut op2_value);
                                self.codegen.rt.set_is_subset.unwrap()
                            }
                            // ast::BinOperand::GreaterOrEqualThan => FloatCC::GreaterThanOrEqual,
                            _ => panic!("Unexpected operator"),
                        };
                        let func_ref = self.get_function_reference(func_id);
                        let call = self.builder().ins().call(func_ref, &[op1_value, op2_value]);
                        let result = {
                            let results = self.builder().inst_results(call);
                            assert!(results.len() == 1, "Invalid number of results");
                            results[0]
                        };
                        self.set_value(id, result);
                    } else {
                        panic!(
                            "Unexpected operator {} for type {}",
                            operator,
                            self.codegen
                                .semantic_context
                                .type_system
                                .get_type_name(op_type)
                        );
                    }
                }
                ast::BinOperand::LogicalAnd => {
                    let result = self.builder().ins().band(lhs_value, rhs_value);
                    self.set_value(id, result);
                }
                ast::BinOperand::InSet => {
                    let func_id = self.codegen.rt.set_contains.unwrap();
                    let func_ref = self.get_function_reference(func_id);
                    let call = self.builder().ins().call(func_ref, &[rhs_value, lhs_value]);
                    let result = {
                        let results = self.builder().inst_results(call);
                        assert!(results.len() == 1, "Invalid number of results");
                        results[0]
                    };
                    self.set_value(id, result);
                }
                _ => {
                    panic!(
                        "Unexpected operator {} for type {}",
                        operator,
                        self.codegen
                            .semantic_context
                            .type_system
                            .get_type_name(op_type)
                    );
                }
            }
        } else if is_set {
            let func_id = match operator {
                ast::BinOperand::Addition => self.codegen.rt.set_union.unwrap(),
                ast::BinOperand::Subtraction => self.codegen.rt.set_difference.unwrap(),
                ast::BinOperand::Multiplication => self.codegen.rt.set_intersection.unwrap(),
                _ => {
                    panic!(
                        "Unexpected operator {} for type {}",
                        operator,
                        self.codegen
                            .semantic_context
                            .type_system
                            .get_type_name(op_type)
                    );
                }
            };
            let func_ref = self.get_function_reference(func_id);
            let call = self.builder().ins().call(func_ref, &[lhs_value, rhs_value]);
            let result = {
                let results = self.builder().inst_results(call);
                assert!(results.len() == 1, "Invalid number of results");
                results[0]
            };
            // Schedule this temporary for disposal.
            self.new_temporary_to_dispose(result, op_type);

            self.set_value(id, result);
        } else {
            panic!("Unexpected case");
        }
        false
    }

    fn visit_pre_expr_conversion(
        &mut self,
        n: &ast::ExprConversion,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        n.0.get().walk_mut(self, n.0.loc(), n.0.id());

        let dest_ty = self.codegen.semantic_context.get_ast_type(id).unwrap();
        let src_ty = self
            .codegen
            .semantic_context
            .get_ast_type(n.0.id())
            .unwrap();

        let src_value = self.get_value(n.0.id());

        if self
            .codegen
            .semantic_context
            .type_system
            .is_real_type(dest_ty)
            && self
                .codegen
                .semantic_context
                .type_system
                .is_integer_type(src_ty)
        {
            let value = self.builder().ins().fcvt_from_sint(F64, src_value);
            self.set_value(id, value);
        } else {
            panic!(
                "Unexpected conversion from {} to {}",
                self.codegen
                    .semantic_context
                    .type_system
                    .get_type_name(src_ty),
                self.codegen
                    .semantic_context
                    .type_system
                    .get_type_name(dest_ty)
            );
        }

        false
    }

    fn visit_pre_stmt_if(
        &mut self,
        n: &ast::StmtIf,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        let cond = &n.0;
        let then_part = &n.1;
        let else_part = &n.2;

        // Evaluate condition.
        cond.get().walk_mut(self, cond.loc(), cond.id());
        self.dispose_temporaries();

        let cond_val = self.get_value(cond.id());

        let then_block = self.builder().create_block();
        let end_if_block = self.builder().create_block();
        let else_block = if else_part.is_some() {
            self.builder().create_block()
        } else {
            end_if_block
        };

        // Jump to then or else (else_block will be end_if_block if there is no else-part)
        self.builder()
            .ins()
            .brif(cond_val, then_block, &[], else_block, &[]);
        self.block_stack.pop();

        // Then
        self.block_stack.push(then_block);
        self.builder().switch_to_block(then_block);

        then_part
            .get()
            .walk_mut(self, then_part.loc(), then_part.id());

        self.builder().ins().jump(end_if_block, &[]);
        self.block_stack.pop();

        // Else (if any)
        if let Some(else_part) = else_part {
            self.builder().switch_to_block(else_block);
            self.block_stack.push(else_block);

            else_part
                .get()
                .walk_mut(self, else_part.loc(), else_part.id());

            self.builder().ins().jump(end_if_block, &[]);
            self.block_stack.pop();
        }

        self.block_stack.push(end_if_block);
        self.builder().switch_to_block(end_if_block);

        false
    }

    fn visit_pre_stmt_repeat_until(
        &mut self,
        n: &ast::StmtRepeatUntil,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        let body = &n.0;
        let cond = &n.1;

        let repeat_block = self.builder().create_block();
        let end_of_repeat = self.builder().create_block();

        self.builder().ins().jump(repeat_block, &[]);
        self.block_stack.pop();

        self.block_stack.push(repeat_block);
        self.builder().switch_to_block(repeat_block);

        body.iter().for_each(|s| {
            s.get().walk_mut(self, s.loc(), s.id());
        });

        cond.get().walk_mut(self, cond.loc(), cond.id());
        self.dispose_temporaries();

        let cond_value = self.get_value(cond.id());

        self.builder()
            .ins()
            .brif(cond_value, end_of_repeat, &[], repeat_block, &[]);
        self.block_stack.pop();

        self.block_stack.push(end_of_repeat);
        self.builder().switch_to_block(end_of_repeat);

        false
    }

    fn visit_pre_stmt_while_do(
        &mut self,
        n: &ast::StmtWhileDo,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        let cond = &n.0;
        let stmt = &n.1;

        let while_check_block = self.builder().create_block();
        let end_while_block = self.builder().create_block();

        self.builder().ins().jump(while_check_block, &[]);
        self.block_stack.pop();

        // Check
        self.block_stack.push(while_check_block);
        self.builder().switch_to_block(while_check_block);

        cond.get().walk_mut(self, cond.loc(), cond.id());
        self.dispose_temporaries();

        let cond_val = self.get_value(cond.id());

        let while_body_block = self.builder().create_block();
        self.builder()
            .ins()
            .brif(cond_val, while_body_block, &[], end_while_block, &[]);
        self.block_stack.pop();

        // While body
        self.block_stack.push(while_body_block);
        self.builder().switch_to_block(while_body_block);

        stmt.get().walk_mut(self, stmt.loc(), stmt.id());

        self.builder().ins().jump(while_check_block, &[]);
        self.block_stack.pop();

        // End while
        self.block_stack.push(end_while_block);
        self.builder().switch_to_block(end_while_block);

        false
    }

    fn visit_pre_stmt_for(
        &mut self,
        n: &ast::StmtFor,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        let for_kind = &n.0;
        let ind_var = &n.1;
        let start = &n.2;
        let end = &n.3;
        let statement = &n.4;

        // Evaluate start and end
        start.get().walk_mut(self, start.loc(), start.id());
        self.dispose_temporaries();

        end.get().walk_mut(self, end.loc(), end.id());
        self.dispose_temporaries();

        let start_val = self.get_value(start.id());
        let end_val = self.get_value(end.id());

        // Check if the range is empty
        let range_is_empty = self.builder().ins().icmp(
            match for_kind {
                ast::ForKind::To => IntCC::SignedGreaterThan, // start > end
                ast::ForKind::DownTo => IntCC::SignedLessThan, // start < end
            },
            start_val,
            end_val,
        );

        let for_init_block = self.builder().create_block();
        let after_for_block = self.builder().create_block();

        // If the range is empty we are done, otherwise go to the initialization block.
        self.builder()
            .ins()
            .brif(range_is_empty, after_for_block, &[], for_init_block, &[]);
        self.block_stack.pop();

        // Initialization
        self.block_stack.push(for_init_block);
        self.builder().switch_to_block(for_init_block);

        // Compute the address of ind_var
        ind_var.get().walk_mut(self, ind_var.loc(), ind_var.id());
        let addr_ind_var = self.get_value(ind_var.id());

        let ind_var_ty = self
            .codegen
            .semantic_context
            .get_ast_type(ind_var.id())
            .unwrap();

        // Initialize induction var with the value of start
        self.builder().ins().store(
            cranelift_codegen::ir::MemFlags::new(),
            start_val,
            addr_ind_var,
            0,
        );

        // Now we can move onto the for block.
        let for_block = self.builder().create_block();
        self.builder().ins().jump(for_block, &[]);
        self.block_stack.pop();

        // Main for block
        self.block_stack.push(for_block);
        self.builder().switch_to_block(for_block);

        // Emit the body of the for
        statement
            .get()
            .walk_mut(self, statement.loc(), statement.id());

        // Load the value of the induction variable
        let cranelift_ty = self.codegen.type_to_cranelift_type(ind_var_ty);
        let ind_var_value = self.builder().ins().load(
            cranelift_ty,
            cranelift_codegen::ir::MemFlags::new(),
            addr_ind_var,
            0,
        );

        // Compute ind_var == end
        let we_are_done = self
            .builder()
            .ins()
            .icmp(IntCC::Equal, ind_var_value, end_val);

        let for_increment_block = self.builder().create_block();
        // If we are done, otherwise go back to the increment block
        self.builder()
            .ins()
            .brif(we_are_done, after_for_block, &[], for_increment_block, &[]);
        self.block_stack.pop();

        // Compute the value of the induction variable for the next iteration.
        self.block_stack.push(for_increment_block);
        self.builder().switch_to_block(for_increment_block);

        let next_ind_var_value = match for_kind {
            ast::ForKind::To => self.builder().ins().iadd_imm(ind_var_value, 1),
            ast::ForKind::DownTo => self.builder().ins().iadd_imm(ind_var_value, -1),
        };

        // Update the induction variable.
        self.builder().ins().store(
            cranelift_codegen::ir::MemFlags::new(),
            next_ind_var_value,
            addr_ind_var,
            0,
        );

        // Jump back.
        self.builder().ins().jump(for_block, &[]);
        self.block_stack.pop();

        self.block_stack.push(after_for_block);
        self.builder().switch_to_block(after_for_block);

        false
    }

    fn visit_pre_stmt_case(
        &mut self,
        n: &ast::StmtCase,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        let case_expr = &n.0;

        case_expr
            .get()
            .walk_mut(self, case_expr.loc(), case_expr.id());
        self.dispose_temporaries();

        let case_val = self.get_value(case_expr.id());

        let case_elems = &n.1;

        // Emit first the switch itself.
        let mut switch = cranelift_frontend::Switch::new();
        let case_block_stmts: Vec<_> = case_elems
            .iter()
            .map(|case_elem| {
                let case_elem = case_elem.get();

                let case_block = self.builder().create_block();

                let case_stmt = &case_elem.1;

                let case_consts = &case_elem.0;
                let case_const_vals: Vec<_> = case_consts
                    .iter()
                    .map(
                        |x| match self.codegen.semantic_context.get_ast_value(x.id()).unwrap() {
                            pasko_frontend::constant::Constant::Integer(x) => x,
                            _ => panic!("Unexpected value"),
                        },
                    )
                    .collect();

                case_const_vals.iter().for_each(|x| {
                    switch.set_entry(*x as u128, case_block);
                });

                (case_stmt, case_block)
            })
            .collect();

        let after_case = self.builder().create_block();
        let otherwise = self.builder().create_block();
        switch.emit(self.builder(), case_val, otherwise);

        // Now emit code for the blocks.
        for case_block_stmt in case_block_stmts {
            let case_stmt = case_block_stmt.0;
            let case_block = case_block_stmt.1;

            self.block_stack.pop();
            self.block_stack.push(case_block);

            self.builder().switch_to_block(case_block);

            case_stmt
                .get()
                .walk_mut(self, case_stmt.loc(), case_stmt.id());
            self.builder().ins().jump(after_case, &[]);
        }

        self.block_stack.pop();
        self.block_stack.push(otherwise);
        self.builder().switch_to_block(otherwise);

        // The "otherwise" block is an error in Basic Pascal.
        // FIXME: This is a bit radical, think of something more user-friendly.
        self.builder()
            .ins()
            .trap(cranelift_codegen::ir::TrapCode::UnreachableCodeReached);

        self.block_stack.pop();
        self.block_stack.push(after_case);

        self.builder().switch_to_block(after_case);

        false
    }

    fn visit_pre_variable_declaration(
        &mut self,
        n: &ast::VariableDeclaration,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        for sym in n.0.iter() {
            let sym_id = self
                .codegen
                .semantic_context
                .get_ast_symbol(sym.id())
                .unwrap();
            let sym = self.codegen.semantic_context.get_symbol(sym_id);
            let sym = sym.borrow();
            let ty = sym.get_type().unwrap();

            if self
                .codegen
                .semantic_context
                .type_system
                .is_integer_type(ty)
                || self.codegen.semantic_context.type_system.is_real_type(ty)
                || self.codegen.semantic_context.type_system.is_bool_type(ty)
                || self.codegen.semantic_context.type_system.is_char_type(ty)
                || self.codegen.semantic_context.type_system.is_enum_type(ty)
                || self
                    .codegen
                    .semantic_context
                    .type_system
                    .is_subrange_type(ty)
                || self.codegen.semantic_context.type_system.is_array_type(ty)
                || self.codegen.semantic_context.type_system.is_record_type(ty)
                || self
                    .codegen
                    .semantic_context
                    .type_system
                    .is_pointer_type(ty)
            {
                self.allocate_value_in_stack(sym_id);
            } else if self.codegen.semantic_context.type_system.is_set_type(ty) {
                self.allocate_value_in_stack(sym_id);
                let location = self.codegen.data_location.get(&sym_id).unwrap().clone();
                // Initialize
                match location {
                    DataLocation::StackVarValue(stack_slot) => {
                        let pointer_type = self.codegen.pointer_type;
                        let stack_addr =
                            self.builder().ins().stack_addr(pointer_type, stack_slot, 0);
                        let null_ptr = self.emit_const_integer(0);
                        self.builder().ins().store(
                            cranelift_codegen::ir::MemFlags::new(),
                            null_ptr,
                            stack_addr,
                            0,
                        );

                        self.add_symbol_to_dispose(sym_id);
                    }
                    _ => {
                        panic!("Unexpected location");
                    }
                }
            } else {
                panic!(
                    "Unexpected type '{}' in variable declaration",
                    self.codegen.semantic_context.type_system.get_type_name(ty)
                );
            }
        }

        false
    }

    fn visit_pre_expr_function_call(
        &mut self,
        n: &ast::ExprFunctionCall,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        let args = &n.1;

        args.iter().for_each(|arg| {
            arg.get().walk_mut(self, arg.loc(), arg.id());
        });

        let callee = &n.0;
        let callee_sym_id = self
            .codegen
            .semantic_context
            .get_ast_symbol(callee.id())
            .unwrap();
        let callee_sym = self.codegen.semantic_context.get_symbol(callee_sym_id);
        let callee_sym = callee_sym.borrow();
        let callee_parameters = callee_sym.get_formal_parameters().unwrap();
        let callee_return_sym = callee_sym.get_return_symbol().unwrap();
        let callee_return_sym = self.codegen.semantic_context.get_symbol(callee_return_sym);
        let callee_return_sym = callee_return_sym.borrow();
        let callee_return_type = callee_return_sym.get_type().unwrap();
        let return_type_is_simple = self
            .codegen
            .semantic_context
            .type_system
            .is_simple_type(callee_return_type)
            || self
                .codegen
                .semantic_context
                .type_system
                .is_pointer_type(callee_return_type);

        let func_id = *self
            .codegen
            .function_identifiers
            .get(&callee_sym_id)
            .unwrap();

        let func_ref = self.get_function_reference(func_id);

        assert!(args.len() == callee_parameters.len());
        let args = args
            .iter()
            .zip(callee_parameters)
            .map(|(value, param)| {
                let param_symbol = self.codegen.semantic_context.get_symbol(param);
                let param_symbol = param_symbol.borrow();
                (value, param_symbol.get_parameter().unwrap())
            })
            .collect::<Vec<_>>();

        let (arg_returns, return_stack_addr) = if return_type_is_simple {
            (vec![], None)
        } else {
            // Special treatment for the return when it is not simple.
            let return_type_size = self.codegen.size_in_bytes(callee_return_type);
            let stack_slot = self.allocate_storage_in_stack(return_type_size as u32);
            let pointer = self.codegen.pointer_type;
            let stack_addr = self.builder().ins().stack_addr(pointer, stack_slot, 0);
            (vec![stack_addr], Some(stack_addr))
        };

        let arg_parameters = self.call_pass_arguments(args);

        let arg_values = arg_returns
            .iter()
            .chain(arg_parameters.iter())
            .copied()
            .collect::<Vec<_>>();

        let call = self.builder().ins().call(func_ref, arg_values.as_slice());
        let result = {
            let results = self.builder().inst_results(call);
            if return_type_is_simple {
                assert!(results.len() == 1, "Invalid number of results");
                results[0]
            } else {
                assert!(results.len() == 0, "Invalid number of results");
                return_stack_addr.unwrap()
            }
        };

        self.value_map.insert(id, result);

        // Temporaries of arguments cannot be disposed until we have returned from the function.
        self.dispose_temporaries();

        false
    }
}

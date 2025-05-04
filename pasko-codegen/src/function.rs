#![allow(unused_imports)]

use cranelift_codegen::ir::function;
use cranelift_codegen::ir::StackSlot;
use cranelift_codegen::ir::Value;
use cranelift_codegen::settings::Configurable;
use cranelift_object::object::elf::NT_SOLARIS_PAGESIZE_HINT;
use cranelift_object::object::pe::IMAGE_SYM_TYPE_INT;
use cranelift_object::object::read::elf::ElfSectionIterator32;
use cranelift_object::object::xcoff::SIZEOF_SYMBOL;
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
use pasko_frontend::symbol::SymbolId;
use pasko_frontend::typesystem::TypeId;
use pasko_frontend::visitor::MutatingVisitable;
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
use std::path::PathBuf;
use std::thread::current;

use crate::datalocation::DataLocation;
use crate::program::CodegenVisitor;

use crate::runtime::RuntimeFunctionId;

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

    enclosing_environment: Option<cranelift_codegen::ir::Value>,

    labeled_blocks: HashMap<usize, cranelift_codegen::ir::Block>,

    variable_counter: usize,

    variables_to_reload: Vec<SymbolId>,
}

enum AddressOrVariable {
    Variable(
        cranelift_frontend::Variable,
        pasko_frontend::symbol::SymbolId,
    ),
    Address(cranelift_codegen::ir::Value),
}

impl<'a, 'b, 'c> FunctionCodegenVisitor<'a, 'b, 'c> {
    pub fn init_function(&mut self, function_symbol_id: Option<symbol::SymbolId>) {
        let entry_block = self.builder().create_block();
        self.entry_block = Some(entry_block);
        self.builder()
            .append_block_params_for_function_params(entry_block);

        self.block_stack.push(entry_block);
        self.builder().switch_to_block(entry_block);

        if let Some(function_symbol_id) = function_symbol_id {
            // If this is a nested function, remember the enclosing environment
            // that will always be passed as the first parameter of the
            // function.
            if self.codegen.is_nested_function(function_symbol_id) {
                self.enclosing_environment = Some(self.builder().block_params(entry_block)[0]);
            }
        }
    }

    pub fn get_entry_block(&self) -> Option<cranelift_codegen::ir::Block> {
        self.entry_block
    }

    pub fn get_codegen(&mut self) -> &mut CodegenVisitor<'b> {
        self.codegen
    }

    pub fn finish_function(&mut self) {
        self.block_stack.pop();
        assert!(self.block_stack.is_empty());

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
            Constant::Integer(x) => self.emit_const_integer(x),
            Constant::Real(x) => self.emit_const_real(x),
            Constant::Bool(x) => self.emit_const_bool(x),
            Constant::String(s) => self.emit_string_literal(&s),
        }
    }

    pub fn emit_stack_ptr_array_null_ended(
        &mut self,
        values: &[cranelift_codegen::ir::Value],
        name: &str,
    ) -> cranelift_codegen::ir::Value {
        let stack_slot = self.allocate_storage_in_stack(
            self.codegen.pointer_type.bytes() * (values.len() + 1) as u32,
        );

        self.codegen
            .annotations
            .new_stack_slot(stack_slot, &format!("[null-ended-array: {}]", name));

        let pointer_type = self.codegen.pointer_type;
        for (idx, v) in values.iter().enumerate() {
            self.builder().ins().stack_store(
                *v,
                stack_slot,
                (pointer_type.bytes() * idx as u32) as i32,
            );
        }

        // End with zero.
        let zero = self.emit_const_integer(0);
        self.builder().ins().stack_store(
            zero,
            stack_slot,
            (pointer_type.bytes() * values.len() as u32) as i32,
        );

        self.builder().ins().stack_addr(pointer_type, stack_slot, 0)
    }

    pub fn emit_string_literal(&mut self, s: &str) -> cranelift_codegen::ir::Value {
        let data_id = match self.codegen.string_table.entry(s.to_owned()) {
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

                self.codegen
                    .global_names
                    .insert(data_id, format!("[string: '{}']", s));

                let mut data_desc = cranelift_module::DataDescription::new();
                let mut unicode_points = s.chars().map(u32::from).collect::<Vec<_>>();
                // FIXME: We should not need to emit a NULL byte anymore.
                unicode_points.push(0); // NULL.
                let bytes = unicode_points
                    .iter()
                    // FIXME: endianness is dependent of the platform
                    .flat_map(|x| x.to_le_bytes())
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
        v
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
            enclosing_environment: None,
            labeled_blocks: HashMap::new(),
            variable_counter: 0,
            variables_to_reload: vec![],
        }
    }

    pub fn get_new_variable(&mut self) -> cranelift_frontend::Variable {
        let v = cranelift_frontend::Variable::new(self.var_counter);

        self.var_counter += 1;

        v
    }

    pub fn allocate_storage_in_stack(&mut self, size: u32) -> StackSlot {
        self.builder_obj
            .as_mut()
            .unwrap()
            .create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                size,
                /* 64 */ 3,
            ))
    }

    pub fn allocate_storage_for_type_in_stack(&mut self, ty: TypeId) -> StackSlot {
        let size = self.codegen.size_in_bytes(ty) as u32;

        self.allocate_storage_in_stack(size)
    }

    fn link_symbol_to_value_stack_slot(
        &mut self,
        sym_id: pasko_frontend::symbol::SymbolId,
        stack_slot: StackSlot,
        annotation_prefix: &str,
    ) {
        let symbol = self.codegen.semantic_context.get_symbol(sym_id);
        let symbol = symbol.borrow();

        self.codegen.annotations.new_stack_slot(
            stack_slot,
            &format!("{}{}", annotation_prefix, symbol.get_name()),
        );

        self.codegen
            .data_location
            .insert(sym_id, DataLocation::StackVarValue(stack_slot));
    }

    pub fn allocate_function_address_in_stack(&mut self, sym_id: pasko_frontend::symbol::SymbolId) {
        // One for the function address and another for the environment.
        let size = self.codegen.pointer_type.bytes() * 2;
        let stack_slot = self.allocate_storage_in_stack(size);

        self.link_symbol_to_value_stack_slot(sym_id, stack_slot, "[function] ");
    }

    pub fn next_variable(&mut self) -> usize {
        self.variable_counter += 1;
        self.variable_counter
    }

    pub fn allocate_variable(&mut self, sym_id: pasko_frontend::symbol::SymbolId) {
        let symbol = self.codegen.semantic_context.get_symbol(sym_id);
        let symbol = symbol.borrow();
        let symbol_type = symbol.get_type().unwrap();

        let new_variable = Variable::new(self.next_variable());
        let ty = self.codegen.type_to_cranelift_type(symbol_type);
        self.builder().declare_var(new_variable, ty);

        self.codegen
            .data_location
            .insert(sym_id, DataLocation::Variable(new_variable, None));
    }

    pub fn allocate_variable_address(&mut self, sym_id: pasko_frontend::symbol::SymbolId) {
        let new_variable = Variable::new(self.next_variable());
        let pointer_type = self.codegen.pointer_type;
        self.builder().declare_var(new_variable, pointer_type);

        self.codegen
            .data_location
            .insert(sym_id, DataLocation::VariableAddress(new_variable));
    }

    pub fn allocate_value_in_stack(&mut self, sym_id: pasko_frontend::symbol::SymbolId) {
        let symbol = self.codegen.semantic_context.get_symbol(sym_id);
        let symbol = symbol.borrow();
        let symbol_type = symbol.get_type().unwrap();

        let stack_slot = self.allocate_storage_for_type_in_stack(symbol_type);

        self.link_symbol_to_value_stack_slot(sym_id, stack_slot, "");
    }

    pub fn allocate_address_in_stack(&mut self, sym_id: pasko_frontend::symbol::SymbolId) {
        let size = self.codegen.pointer_type.bytes();

        let stack_slot = self.allocate_storage_in_stack(size);

        let symbol = self.codegen.semantic_context.get_symbol(sym_id);
        let symbol = symbol.borrow();

        self.codegen.annotations.new_stack_slot(
            stack_slot,
            &format!("[by reference parameter] {}", symbol.get_name()),
        );

        self.codegen
            .data_location
            .insert(sym_id, DataLocation::StackVarAddress(stack_slot));
    }

    pub fn add_offset_to_address(
        &mut self,
        addr: cranelift_codegen::ir::Value,
        offset: i64,
    ) -> cranelift_codegen::ir::Value {
        if offset == 0 {
            addr
        } else {
            let offset = self.emit_const_integer(offset);
            self.builder().ins().iadd(addr, offset)
        }
    }

    // FIXME: Adapted from visit_pre_stmt_for
    // Maybe we can generalise this one and use it there.
    fn emit_counted_loop<F>(
        &mut self,
        start_val: cranelift_codegen::ir::Value,
        end_val: cranelift_codegen::ir::Value,
        body: F,
    ) where
        F: Fn(&mut Self, cranelift_codegen::ir::Value),
    {
        let ind_var = self.get_new_variable();
        self.builder().declare_var(ind_var, I64);

        let range_is_empty = self.builder().ins().icmp(
            IntCC::SignedGreaterThan, // start > end
            start_val,
            end_val,
        );

        let for_init_block = self.builder().create_block();
        let after_for_block = self.builder().create_block();

        // If the range is empty we are done, otherwise go to the initialization block.
        self.builder()
            .ins()
            .brif(range_is_empty, after_for_block, &[], for_init_block, &[]);
        self.block_stack.pop(); // whatever block was before we entered this loop.

        // Initialization
        self.block_stack.push(for_init_block);
        self.builder().switch_to_block(for_init_block);

        // Initialize induction var with the value of start
        self.builder().def_var(ind_var, start_val);

        let for_block = self.builder().create_block();
        self.builder().ins().jump(for_block, &[]);
        self.block_stack.pop(); // for_init_block

        // Main for block
        self.block_stack.push(for_block);
        self.builder().switch_to_block(for_block);

        let idx_val = self.builder().use_var(ind_var);
        let idx_val = self.builder().ins().isub(idx_val, start_val);

        body(self, idx_val);

        let ind_var_value = self.builder().use_var(ind_var);

        let we_are_done = self
            .builder()
            .ins()
            .icmp(IntCC::Equal, ind_var_value, end_val);

        let for_increment_block = self.builder().create_block();
        self.builder()
            .ins()
            .brif(we_are_done, after_for_block, &[], for_increment_block, &[]);
        self.block_stack.pop(); // main_for_block

        // Compute the value of the induction variable for the next iteration.
        self.block_stack.push(for_increment_block);
        self.builder().switch_to_block(for_increment_block);

        let next_ind_var_value = self.builder().ins().iadd_imm(ind_var_value, 1);

        // Update the induction variable.
        self.builder().def_var(ind_var, next_ind_var_value);

        // Jump back.
        self.builder().ins().jump(for_block, &[]);
        self.block_stack.pop(); // for_increment_block

        // After the loop.
        self.block_stack.push(after_for_block);
        self.builder().switch_to_block(after_for_block);
    }

    fn store_value_into_address_traversal(
        &mut self,
        addr: cranelift_codegen::ir::Value,
        addr_ty: pasko_frontend::typesystem::TypeId,
        value: cranelift_codegen::ir::Value,
        value_ty: pasko_frontend::typesystem::TypeId,
        is_initialization: bool,
        value_is_variable: bool,
    ) {
        if self
            .codegen
            .semantic_context
            .type_system
            .is_simple_type(addr_ty)
        {
            self.builder()
                .ins()
                .store(cranelift_codegen::ir::MemFlags::new(), value, addr, 0);
        } else if self
            .codegen
            .semantic_context
            .type_system
            .is_array_type(addr_ty)
        {
            if !self.codegen.type_contains_set_types(addr_ty) {
                let size = self.emit_const_integer(self.codegen.size_in_bytes(addr_ty) as i64);
                let target_config = {
                    let object_module = self.codegen.object_module.as_ref().unwrap();
                    object_module.target_config()
                };
                assert!(
                    self.codegen.size_in_bytes(addr_ty) == self.codegen.size_in_bytes(value_ty),
                    "Type sizes must match"
                );
                self.builder().call_memcpy(target_config, addr, value, size);
            } else {
                // In this case, use a loop to emit one store for each element.
                let index_ty = self
                    .codegen
                    .semantic_context
                    .type_system
                    .array_type_get_index_type(addr_ty);

                let lower_bound = self
                    .codegen
                    .semantic_context
                    .type_system
                    .ordinal_type_lower_bound(index_ty);
                let upper_bound = self
                    .codegen
                    .semantic_context
                    .type_system
                    .ordinal_type_upper_bound(index_ty);

                let element_ty = self
                    .codegen
                    .semantic_context
                    .type_system
                    .array_type_get_component_type(addr_ty);

                let lower_bound = self.emit_const_integer(lower_bound);
                let upper_bound = self.emit_const_integer(upper_bound);

                let size_in_bytes = self.codegen.size_in_bytes(element_ty) as i64;
                let size_in_bytes = self.emit_const_integer(size_in_bytes);

                // Emit a loop to avoid code bloat when the extent of the array is big.
                self.emit_counted_loop(lower_bound, upper_bound, |self_, idx| {
                    let offset = self_.builder().ins().imul(idx, size_in_bytes);

                    let element_addr = self_.builder().ins().iadd(addr, offset);

                    // This works because the value of an array in cranelift is its address.
                    let element_value = self_.builder().ins().iadd(value, offset);
                    let element_value = self_.load_value_from_address(element_value, element_ty);

                    self_.store_value_into_address_traversal(
                        element_addr,
                        element_ty,
                        element_value,
                        element_ty,
                        is_initialization,
                        value_is_variable,
                    );
                });
            }
        } else if self
            .codegen
            .semantic_context
            .type_system
            .is_record_type(addr_ty)
        {
            if !self.codegen.type_contains_set_types(addr_ty) {
                let size = self.emit_const_integer(self.codegen.size_in_bytes(addr_ty) as i64);
                let target_config = {
                    let object_module = self.codegen.object_module.as_ref().unwrap();
                    object_module.target_config()
                };
                assert!(
                    self.codegen.size_in_bytes(addr_ty) == self.codegen.size_in_bytes(value_ty),
                    "Type sizes must match"
                );
                self.builder().call_memcpy(target_config, addr, value, size);
            } else {
                // In this case, fall back to element wise store which is always
                // correct (even if not as efficient as a single bulk memcpy).
                let fields = self
                    .codegen
                    .semantic_context
                    .type_system
                    .record_type_get_fixed_fields(addr_ty);
                if self
                    .codegen
                    .semantic_context
                    .type_system
                    .record_type_get_variant_part(addr_ty)
                    .is_some()
                {
                    unimplemented!("Variant types");
                }
                fields.iter().for_each(|field_id| {
                    let offset = self.get_offset_of_field(addr_ty, *field_id);

                    let field_sym = self.codegen.semantic_context.get_symbol(*field_id);
                    let field_sym = field_sym.borrow();
                    let field_ty = field_sym.get_type().unwrap();

                    let field_addr = self.add_offset_to_address(addr, offset);

                    // This works because for records, the cranelift value should already be an address.
                    let field_value = self.add_offset_to_address(value, offset);
                    let field_value = self.load_value_from_address(field_value, field_ty);

                    self.store_value_into_address_traversal(
                        field_addr,
                        field_ty,
                        field_value,
                        field_ty,
                        is_initialization,
                        value_is_variable,
                    );
                });
            }
        } else if self
            .codegen
            .semantic_context
            .type_system
            .is_set_type(addr_ty)
        {
            if !is_initialization {
                // To assign we need first to free the lhs and then copy, if the right hand side is a temporary we can just move the value.
                let pointer_type = self.codegen.pointer_type;
                let pointer_value = self.builder().ins().load(
                    pointer_type,
                    cranelift_codegen::ir::MemFlags::new(),
                    addr,
                    0,
                );
                self.dispose_set_variable(pointer_value);
            }

            let src_value = if value_is_variable {
                // We need to copy the pointer from this variable.
                let func_id = self
                    .codegen
                    .get_runtime_function(RuntimeFunctionId::SetCopy);
                let func_ref = self.get_function_reference(func_id);
                let call = self.builder().ins().call(func_ref, &[value]);

                let result = {
                    let results = self.builder().inst_results(call);
                    assert!(results.len() == 1, "Invalid number of results");
                    results[0]
                };

                result
            } else {
                value
            };

            // Finally, store the address of the rhs set.
            self.builder()
                .ins()
                .store(cranelift_codegen::ir::MemFlags::new(), src_value, addr, 0);
        } else if self
            .codegen
            .semantic_context
            .type_system
            .is_pointer_type(addr_ty)
        {
            self.builder()
                .ins()
                .store(cranelift_codegen::ir::MemFlags::new(), value, addr, 0);
        } else {
            panic!(
                "Do not know how to assign a value of type {}",
                self.codegen
                    .semantic_context
                    .type_system
                    .get_type_name(addr_ty)
            );
        }
    }

    fn store_value_into_address(
        &mut self,
        addr: cranelift_codegen::ir::Value,
        addr_ty: pasko_frontend::typesystem::TypeId,
        value: cranelift_codegen::ir::Value,
        value_ty: pasko_frontend::typesystem::TypeId,
        is_initialization: bool,
        value_is_variable: bool,
    ) {
        self.store_value_into_address_traversal(
            addr,
            addr_ty,
            value,
            value_ty,
            is_initialization,
            value_is_variable,
        );

        // Only top-level values are proper temporaries.
        if self.codegen.type_contains_set_types(value_ty) && !value_is_variable {
            assert!(self.codegen.type_contains_set_types(addr_ty));
            self.remove_temporary_to_dispose(value);
        }
    }

    fn store_value_into_address_for_assignment(
        &mut self,
        addr: cranelift_codegen::ir::Value,
        addr_ty: pasko_frontend::typesystem::TypeId,
        value: cranelift_codegen::ir::Value,
        value_ty: pasko_frontend::typesystem::TypeId,
    ) {
        // value_is_variable is set to true so we will always copy rather than move a handle.
        self.store_value_into_address(addr, addr_ty, value, value_ty, false, true);
    }

    fn expr_is_variable(&self, expr_value: &span::SpannedBox<ast::Expr>) -> bool {
        matches!(
            expr_value.get(),
            ast::Expr::Variable(..) | ast::Expr::VariableReference(..)
        )
    }

    // This function does not walk expr_value!
    fn store_expr_into_address_for_assignment(
        &mut self,
        addr: cranelift_codegen::ir::Value,
        addr_ty: pasko_frontend::typesystem::TypeId,
        expr_value: &span::SpannedBox<ast::Expr>,
    ) {
        let value_is_variable = self.expr_is_variable(expr_value);

        let value = self.get_value(expr_value.id());
        let value_ty = self
            .codegen
            .semantic_context
            .get_ast_type(expr_value.id())
            .unwrap();

        self.store_value_into_address(addr, addr_ty, value, value_ty, false, value_is_variable);
    }

    fn store_expr_into_address_for_initialization(
        &mut self,
        addr: cranelift_codegen::ir::Value,
        addr_ty: pasko_frontend::typesystem::TypeId,
        expr_value: &span::SpannedBox<ast::Expr>,
    ) {
        let value_is_variable = self.expr_is_variable(expr_value);

        let value = self.get_value(expr_value.id());
        let value_ty = self
            .codegen
            .semantic_context
            .get_ast_type(expr_value.id())
            .unwrap();

        self.store_value_into_address(addr, addr_ty, value, value_ty, true, value_is_variable);
    }

    fn load_value_from_address(
        &mut self,
        addr: cranelift_codegen::ir::Value,
        addr_ty: pasko_frontend::typesystem::TypeId,
    ) -> cranelift_codegen::ir::Value {
        if self
            .codegen
            .semantic_context
            .type_system
            .is_simple_type(addr_ty)
        {
            let cranelift_ty = self.codegen.type_to_cranelift_type(addr_ty);

            let v = self.builder().ins().load(
                cranelift_ty,
                cranelift_codegen::ir::MemFlags::new(),
                addr,
                0,
            );

            v
        } else if self
            .codegen
            .semantic_context
            .type_system
            .is_array_type(addr_ty)
            || self
                .codegen
                .semantic_context
                .type_system
                .is_conformable_array_type(addr_ty)
            || self
                .codegen
                .semantic_context
                .type_system
                .is_record_type(addr_ty)
        {
            // Structured types cannot have value semantics in the cranelift IR.
            addr
        } else if self
            .codegen
            .semantic_context
            .type_system
            .is_set_type(addr_ty)
            || self
                .codegen
                .semantic_context
                .type_system
                .is_file_type(addr_ty)
        {
            // Set and file types types are opaque pointers so in some sense they're like simple types
            // but with an opaque pointer type.
            let pointer_type = self.codegen.pointer_type;
            let v = self.builder().ins().load(
                pointer_type,
                cranelift_codegen::ir::MemFlags::new(),
                addr,
                0,
            );

            v
        } else if self
            .codegen
            .semantic_context
            .type_system
            .is_pointer_type(addr_ty)
        {
            let pointer_type = self.codegen.pointer_type;
            let v = self.builder().ins().load(
                pointer_type,
                cranelift_codegen::ir::MemFlags::new(),
                addr,
                0,
            );
            v
        } else {
            panic!(
                "Unhandled type {} in variable reference",
                self.codegen
                    .semantic_context
                    .type_system
                    .get_type_name(addr_ty)
            );
        }
    }

    fn emit_conversion(
        &mut self,
        dest_ty: pasko_frontend::typesystem::TypeId,
        src_ty: pasko_frontend::typesystem::TypeId,
        src_value: cranelift_codegen::ir::Value,
    ) -> cranelift_codegen::ir::Value {
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
            self.builder().ins().fcvt_from_sint(F64, src_value)
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
    }

    pub fn link_bound_identifier(
        &mut self,
        param_idx: usize,
        sym_id: pasko_frontend::symbol::SymbolId,
    ) {
        let builder = self.builder_obj.as_mut().unwrap();
        let param_value = builder.block_params(self.entry_block.unwrap())[param_idx];

        self.codegen
            .data_location
            .insert(sym_id, DataLocation::Value(param_value));
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
            DataLocation::Variable(var, ..) => {
                builder.def_var(*var, param_value);
                let sym = self.codegen.semantic_context.get_symbol(sym_id);
                let sym = sym.borrow();
                self.codegen
                    .annotations
                    .new_value(param_value, sym.get_name());
                // And we are done because variables do not have address.
                return;
            }
            DataLocation::VariableAddress(var) => {
                builder.def_var(*var, param_value);
                let sym = self.codegen.semantic_context.get_symbol(sym_id);
                let sym = sym.borrow();
                self.codegen
                    .annotations
                    .new_value(param_value, sym.get_name());
                // And we are done because variables do not have address.
                return;
            }
            _ => {
                panic!("Unexpected data location for parameter");
            }
        };
        let builder = self.builder_obj.as_mut().unwrap();
        builder.ins().stack_store(param_value, stack_slot, 0);
    }

    pub fn copy_in_function_functional_parameter(
        &mut self,
        param_idx: usize,
        sym_id: pasko_frontend::symbol::SymbolId,
    ) {
        let builder = self.builder_obj.as_mut().unwrap();
        let function_address = builder.block_params(self.entry_block.unwrap())[param_idx];
        let environment_address = builder.block_params(self.entry_block.unwrap())[param_idx + 1];

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

        builder.ins().stack_store(function_address, stack_slot, 0);
        builder.ins().stack_store(
            environment_address,
            stack_slot,
            self.codegen.pointer_type.bytes() as i32,
        );
    }

    pub fn get_value_of_variable(
        &mut self,
        sym_id: pasko_frontend::symbol::SymbolId,
    ) -> cranelift_codegen::ir::Value {
        let location = *self.codegen.data_location.get(&sym_id).unwrap();

        match location {
            DataLocation::Variable(var, ..) => {
                let v = self.builder().use_var(var);
                let symbol = self.codegen.semantic_context.get_symbol(sym_id);
                let symbol = symbol.borrow();
                self.codegen.annotations.new_value(v, symbol.get_name());
                v
            }
            _ => panic!("This is not a variable"),
        }
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

                    builder.ins().stack_load(cranelift_ty, *stack_slot, 0)
                } else if self.codegen.semantic_context.type_system.is_array_type(ty)
                    || self.codegen.semantic_context.type_system.is_record_type(ty)
                {
                    builder
                        .ins()
                        .stack_addr(self.codegen.pointer_type, *stack_slot, 0)
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

                // Load the address in the stack.
                let variable_address =
                    builder
                        .ins()
                        .stack_load(self.codegen.pointer_type, *stack_slot, 0);

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
                let gv = object_module.declare_data_in_func(data_id, func);

                if let Some(global_name) = self.codegen.global_names.get(&data_id) {
                    self.codegen.annotations.new_global_value(gv, global_name);
                }

                entry.insert(gv);
                gv
            }
        };

        gv
    }

    fn get_file_addr(
        &mut self,
        gv: cranelift_codegen::ir::GlobalValue,
    ) -> cranelift_codegen::ir::Value {
        let pointer_type = self.codegen.pointer_type;
        self.builder().ins().global_value(pointer_type, gv)
    }

    fn get_file_val(
        &mut self,
        gv: cranelift_codegen::ir::GlobalValue,
    ) -> cranelift_codegen::ir::Value {
        let pointer_type = self.codegen.pointer_type;
        let addr = self.get_file_addr(gv);
        self.builder().ins().load(
            pointer_type,
            cranelift_codegen::ir::MemFlags::new(),
            addr,
            0,
        )
    }

    pub fn get_input_file_val(&mut self) -> cranelift_codegen::ir::Value {
        let gv = self.get_global_value(self.codegen.get_input_file_data_id());
        self.get_file_val(gv)
    }

    pub fn get_output_file_val(&mut self) -> cranelift_codegen::ir::Value {
        let gv = self.get_global_value(self.codegen.get_output_file_data_id());
        self.get_file_val(gv)
    }

    pub fn get_input_file_addr(&mut self) -> cranelift_codegen::ir::Value {
        let gv = self.get_global_value(self.codegen.get_input_file_data_id());
        self.get_file_addr(gv)
    }

    pub fn get_output_file_addr(&mut self) -> cranelift_codegen::ir::Value {
        let gv = self.get_global_value(self.codegen.get_output_file_data_id());
        self.get_file_addr(gv)
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
                    .declare_func_in_func(func_id, func);

                entry.insert(func_ref);
                func_ref
            }
        };

        // Always insert an annotation because we start afresh for each function.
        if let Some(name) = self.codegen.function_names.get(&func_id) {
            self.codegen.annotations.new_function_ref(func_ref, name);
        }

        func_ref
    }

    fn call_pass_arguments<F>(
        &mut self,
        callee_sym_id: pasko_frontend::symbol::SymbolId,
        args: &[(
            &span::SpannedBox<ast::Expr>,
            pasko_frontend::symbol::SymbolId,
        )],
        environment: Option<F>,
        arg_return: Option<cranelift_codegen::ir::Value>,
    ) -> Vec<cranelift_codegen::ir::Value>
    where
        F: Fn(&mut Self) -> cranelift_codegen::ir::Value,
    {
        let mut result = vec![];
        let needs_environment = {
            if self.codegen.is_nested_function(callee_sym_id) {
                true
            } else {
                let callee_sym = self.codegen.semantic_context.get_symbol(callee_sym_id);
                let callee_sym = callee_sym.borrow();
                // We allow calling functional parameters without environment.
                callee_sym.get_parameter().is_some() && environment.is_some()
            }
        };
        if needs_environment {
            result.push(environment.unwrap()(self));
        }
        if let Some(arg_return) = arg_return {
            result.push(arg_return);
        }
        let mut passed_args: Vec<_> = args
            .iter()
            .flat_map(|item| {
                let (arg, param) = item;

                let param_sym = self.codegen.semantic_context.get_symbol(*param);
                let param_sym = param_sym.borrow();
                let param_kind = param_sym.get_parameter().unwrap();

                match param_kind {
                    ParameterKind::Variable => {
                        let arg_value = *self.value_map.get(&arg.id()).unwrap();
                        vec![arg_value]
                    }
                    ParameterKind::Value => {
                        let arg_value = *self.value_map.get(&arg.id()).unwrap();
                        let arg_ty = self
                            .codegen
                            .semantic_context
                            .get_ast_type(arg.id())
                            .unwrap();
                        if self
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
                            vec![arg_value]
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
                            || self
                                .codegen
                                .semantic_context
                                .type_system
                                .is_set_type(arg_ty)
                        {
                            // We need to do a copy in.
                            let arg_type_size = self.codegen.size_in_bytes(arg_ty);
                            let stack_slot = self.allocate_storage_in_stack(arg_type_size as u32);
                            self.codegen
                                .annotations
                                .new_stack_slot(stack_slot, "[copy-in]");
                            let pointer = self.codegen.pointer_type;
                            let stack_addr =
                                self.builder().ins().stack_addr(pointer, stack_slot, 0);

                            self.store_expr_into_address_for_initialization(
                                stack_addr, arg_ty, arg,
                            );
                            vec![stack_addr]
                        } else {
                            panic!(
                                "Unexpected type in argument pass {}",
                                self.codegen
                                    .semantic_context
                                    .type_system
                                    .get_type_name(arg_ty)
                            );
                        }
                    }
                    ParameterKind::Function | ParameterKind::Procedure => {
                        let function_address = *self.value_map.get(&arg.id()).unwrap();
                        // This is not ideal but this is always a weird case.
                        let ast_id = match arg.get() {
                            ast::Expr::VariableReference(var_ref) => var_ref.0.id(),
                            _ => unreachable!(),
                        };
                        let function_arg_sym_id = self
                            .codegen
                            .semantic_context
                            .get_ast_symbol(ast_id)
                            .unwrap();
                        let function_arg_sym = self
                            .codegen
                            .semantic_context
                            .get_symbol(function_arg_sym_id);
                        let function_arg_sym = function_arg_sym.borrow();

                        let arg_environment = if function_arg_sym.get_parameter().is_none() {
                            self.emit_environment_for_function_reference(function_arg_sym_id)
                        } else {
                            let callee_storage = self.get_address_of_symbol(function_arg_sym_id);

                            let pointer_type = self.codegen.pointer_type;

                            self.builder().ins().load(
                                pointer_type,
                                cranelift_codegen::ir::MemFlags::new(),
                                callee_storage,
                                pointer_type.bytes() as i32,
                            )
                        };

                        vec![function_address, arg_environment]
                    }
                    ParameterKind::ValueConformableArray => {
                        // We need to do a copy in.
                        let arg_ty = self
                            .codegen
                            .semantic_context
                            .get_ast_type(arg.id())
                            .unwrap();
                        let arg_type_size = self.codegen.size_in_bytes(arg_ty);
                        let stack_slot = self.allocate_storage_in_stack(arg_type_size as u32);
                        self.codegen
                            .annotations
                            .new_stack_slot(stack_slot, "[copy-in]");
                        let pointer = self.codegen.pointer_type;
                        let stack_addr = self.builder().ins().stack_addr(pointer, stack_slot, 0);

                        self.store_expr_into_address_for_initialization(stack_addr, arg_ty, arg);
                        vec![stack_addr]
                    }
                    ParameterKind::VariableConformableArray => {
                        let arg_value = *self.value_map.get(&arg.id()).unwrap();
                        vec![arg_value]
                    }
                }
            })
            .collect();

        result.append(&mut passed_args);

        // Now pass the bound identifiers, if any.
        let mut bound_args: Vec<_> = args
            .iter()
            .flat_map(|item| {
                let (arg, param) = item;

                let param_sym = self.codegen.semantic_context.get_symbol(*param);
                let param_sym = param_sym.borrow();
                let param_kind = param_sym.get_parameter().unwrap();

                match param_kind {
                    ParameterKind::ValueConformableArray
                    | ParameterKind::VariableConformableArray => {
                        let mut param_ty = param_sym.get_type().unwrap();
                        let mut arg_ty = self
                            .codegen
                            .semantic_context
                            .get_ast_type(arg.id())
                            .unwrap();
                        let mut result = vec![];
                        while self
                            .codegen
                            .semantic_context
                            .type_system
                            .is_conformable_array_type(param_ty)
                        {
                            if self
                                .codegen
                                .semantic_context
                                .type_system
                                .is_array_type(arg_ty)
                            {
                                let index_type = self
                                    .codegen
                                    .semantic_context
                                    .type_system
                                    .array_type_get_index_type(arg_ty);
                                let lower_bound = self
                                    .codegen
                                    .semantic_context
                                    .type_system
                                    .ordinal_type_lower_bound(index_type);
                                let lower_bound = self.emit_const_integer(lower_bound);
                                let upper_bound = self
                                    .codegen
                                    .semantic_context
                                    .type_system
                                    .ordinal_type_upper_bound(index_type);
                                let upper_bound = self.emit_const_integer(upper_bound);
                                result.push(lower_bound);
                                result.push(upper_bound);
                            } else if self
                                .codegen
                                .semantic_context
                                .type_system
                                .is_conformable_array_type(arg_ty)
                            {
                                if param_kind == ParameterKind::ValueConformableArray {
                                    unreachable!("conformable arrays cannot be passed by value!");
                                }
                                let lower_bound = self
                                    .codegen
                                    .semantic_context
                                    .type_system
                                    .conformable_array_type_get_lower(arg_ty);
                                let lower_bound = self.get_value_of_bound_identifier(lower_bound);
                                let upper_bound = self
                                    .codegen
                                    .semantic_context
                                    .type_system
                                    .conformable_array_type_get_upper(arg_ty);
                                let upper_bound = self.get_value_of_bound_identifier(upper_bound);
                                result.push(lower_bound);
                                result.push(upper_bound);
                            } else {
                                unreachable!("invalid type");
                            }
                            param_ty = self
                                .codegen
                                .semantic_context
                                .type_system
                                .conformable_array_type_get_component_type(param_ty);
                            arg_ty = self
                                .codegen
                                .semantic_context
                                .type_system
                                .array_or_conformable_array_type_get_component_type(arg_ty);
                        }
                        result
                    }
                    _ => {
                        vec![]
                    }
                }
            })
            .collect();

        result.append(&mut bound_args);

        result
    }

    fn emit_if_then_else<FThen, FElse>(
        &mut self,
        cond_val: cranelift_codegen::ir::Value,
        then_part: FThen,
        else_part: Option<FElse>,
    ) where
        FThen: Fn(&mut Self),
        FElse: Fn(&mut Self),
    {
        let then_block = self.builder().create_block();
        let end_if_block = self.builder().create_block();
        let else_block = if else_part.is_some() {
            self.builder().create_block()
        } else {
            end_if_block
        };

        self.builder()
            .ins()
            .brif(cond_val, then_block, &[], else_block, &[]);
        self.block_stack.pop();

        self.block_stack.push(then_block);
        self.builder().switch_to_block(then_block);

        then_part(self);

        self.builder().ins().jump(end_if_block, &[]);
        self.block_stack.pop();

        if let Some(else_part) = else_part {
            self.builder().switch_to_block(else_block);
            self.block_stack.push(else_block);

            else_part(self);

            self.builder().ins().jump(end_if_block, &[]);
            self.block_stack.pop();
        }

        self.block_stack.push(end_if_block);
        self.builder().switch_to_block(end_if_block);
    }

    fn emit_call(
        &mut self,
        callee_sym_id: pasko_frontend::symbol::SymbolId,
        args: Vec<(
            &span::SpannedBox<ast::Expr>,
            pasko_frontend::symbol::SymbolId,
        )>,
        arg_return: Option<cranelift_codegen::ir::Value>,
        wants_return_value: bool,
    ) -> Option<cranelift_codegen::ir::Value> {
        let callee_sym = self.codegen.semantic_context.get_symbol(callee_sym_id);
        let callee_sym = callee_sym.borrow();

        let mut result = None;

        if callee_sym.get_parameter().is_some() {
            let callee_storage = self.get_address_of_symbol(callee_sym_id);
            let pointer_type = self.codegen.pointer_type;

            let environment = self.builder().ins().load(
                pointer_type,
                cranelift_codegen::ir::MemFlags::new(),
                callee_storage,
                pointer_type.bytes() as i32,
            );

            // Check if the environmeent is null. If it is, this function
            // does not need environment.  Note that we pass the environment
            // for all nested functions, even if they don't actually use it
            // at all, so this should be correct.
            let zero = self.builder().ins().iconst(pointer_type, 0);
            let cond_value = self
                .builder()
                .ins()
                .icmp(IntCC::NotEqual, environment, zero);

            let mut return_addr = None;
            let mut return_type = None;
            if wants_return_value {
                assert!(callee_sym.get_kind() == pasko_frontend::symbol::SymbolKind::Function);

                let return_sym_id = callee_sym.get_return_symbol().unwrap();
                let return_sym = self.codegen.semantic_context.get_symbol(return_sym_id);
                let return_sym = return_sym.borrow();

                return_type = return_sym.get_type();

                let ss = self.allocate_storage_for_type_in_stack(return_type.unwrap());
                return_addr = Some(self.builder().ins().stack_addr(pointer_type, ss, 0));
            }

            self.emit_if_then_else(
                cond_value,
                |self_: &mut FunctionCodegenVisitor<'a, 'b, 'c>| {
                    let arg_values = self_.call_pass_arguments(
                        callee_sym_id,
                        &args,
                        Some(|_: &mut Self| environment),
                        arg_return,
                    );

                    let signature = self_
                        .codegen
                        .function_signatures
                        .get(&callee_sym_id)
                        .unwrap()
                        .clone();
                    let function_address = self_.builder().ins().load(
                        pointer_type,
                        cranelift_codegen::ir::MemFlags::new(),
                        callee_storage,
                        0,
                    );
                    let sig_ref = self_.builder().import_signature(signature);
                    let call = self_.builder().ins().call_indirect(
                        sig_ref,
                        function_address,
                        arg_values.as_slice(),
                    );
                    let results = self_.builder().inst_results(call).to_vec();
                    if wants_return_value {
                        assert!(results.len() == 1);
                        self_.store_value_into_address(
                            return_addr.unwrap(),
                            return_type.unwrap(),
                            results[0],
                            return_type.unwrap(),
                            true,
                            false,
                        );
                    } else {
                        assert!(results.is_empty());
                    }
                },
                Some({
                    |self_: &mut FunctionCodegenVisitor<'a, 'b, 'c>| {
                        let arg_values = self_.call_pass_arguments(
                            callee_sym_id,
                            &args,
                            None::<fn(&mut Self) -> cranelift_codegen::ir::Value>,
                            arg_return,
                        );

                        let mut signature = self_
                            .codegen
                            .function_signatures
                            .get(&callee_sym_id)
                            .unwrap()
                            .clone();
                        // Remove the environment parameter;
                        signature.params.remove(0);

                        let function_address = self_.builder().ins().load(
                            pointer_type,
                            cranelift_codegen::ir::MemFlags::new(),
                            callee_storage,
                            0,
                        );
                        let sig_ref = self_.builder().import_signature(signature);
                        let call = self_.builder().ins().call_indirect(
                            sig_ref,
                            function_address,
                            arg_values.as_slice(),
                        );
                        let results = self_.builder().inst_results(call).to_vec();
                        if wants_return_value {
                            assert!(results.len() == 1);
                            self_.store_value_into_address(
                                return_addr.unwrap(),
                                return_type.unwrap(),
                                results[0],
                                return_type.unwrap(),
                                true,
                                false,
                            );
                        } else {
                            assert!(results.is_empty());
                        }
                    }
                }),
            );
            if wants_return_value {
                result =
                    Some(self.load_value_from_address(return_addr.unwrap(), return_type.unwrap()));
            }
        } else {
            let environment =
                |self_: &mut Self| self_.emit_environment_for_function_reference(callee_sym_id);
            let arg_values =
                self.call_pass_arguments(callee_sym_id, &args, Some(environment), arg_return);

            let func_id = *self
                .codegen
                .function_identifiers
                .get(&callee_sym_id)
                .unwrap();

            let func_ref = self.get_function_reference(func_id);
            let call = self.builder().ins().call(func_ref, arg_values.as_slice());
            if wants_return_value {
                let results = self.builder().inst_results(call);
                assert!(results.len() == 1);
                result = Some(results[0]);
            }
        }
        result
    }

    fn emit_procedure_call(
        &mut self,
        callee_sym_id: pasko_frontend::symbol::SymbolId,
        args: Vec<(
            &span::SpannedBox<ast::Expr>,
            pasko_frontend::symbol::SymbolId,
        )>,
    ) {
        self.emit_call(
            callee_sym_id,
            args,
            None,
            /* want_return_value */ false,
        );
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
        let func_id = self
            .codegen
            .get_runtime_function(RuntimeFunctionId::SetDispose);
        let func_ref = self.get_function_reference(func_id);
        self.builder().ins().call(func_ref, &[addr]);
    }

    fn dispose_temporaries(&mut self) {
        let temporaries = std::mem::take(&mut self.temporaries_to_dispose);

        for Temporary { addr, ty } in temporaries {
            self.dispose_var_of_type(addr, ty)
        }
    }

    fn reload_variables(&mut self) {
        let variables_to_reload = std::mem::take(&mut self.variables_to_reload);
        for sym_id in variables_to_reload {
            let dl = *self.codegen.data_location.get(&sym_id).unwrap();
            match dl {
                DataLocation::Variable(var, Some(address)) => {
                    let sym = self.codegen.semantic_context.get_symbol(sym_id);
                    let sym = sym.borrow();
                    let ty = sym.get_type().unwrap();
                    let val = self.load_value_from_address(address, ty);
                    self.builder().def_var(var, val);
                    self.codegen.annotations.new_value(val, sym.get_name());
                }
                _ => panic!("Unexpected data location"),
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
            DataLocation::VariableAddress(var) => {
                let addr = self.builder().use_var(var);
                addr
            }
            DataLocation::StackVarValue(stack_slot) => {
                let pointer = self.codegen.pointer_type;
                let addr = self.builder().ins().stack_addr(pointer, stack_slot, 0);

                addr
            }
            DataLocation::StackVarAddress(stack_slot) => {
                let pointer_type = self.codegen.pointer_type;
                let addr = self.builder().ins().stack_load(pointer_type, stack_slot, 0);
                addr
            }
            DataLocation::NestedVarValue {
                env_levels_up,
                env_var_index,
            } => {
                let pointer_type = self.codegen.pointer_type;
                // This is an address
                let mut enclosing_environment = self.enclosing_environment.unwrap();

                // Move up the static chain.
                (0..env_levels_up).for_each(|_idx| {
                    enclosing_environment = self.builder().ins().load(
                        pointer_type,
                        cranelift_codegen::ir::MemFlags::new(),
                        enclosing_environment,
                        0,
                    )
                });

                // TODO: Traverse nesting up if larger than 1.
                // +1
                let addr = self.builder().ins().load(
                    pointer_type,
                    cranelift_codegen::ir::MemFlags::new(),
                    enclosing_environment,
                    pointer_type.bytes() as i32 * (env_var_index as i32 + 1),
                );

                addr
            }
            _ => {
                panic!("Cannot get the address of {:?}", storage)
            }
        }
    }

    pub fn get_address_of_symbol(
        &mut self,
        sym_id: pasko_frontend::symbol::SymbolId,
    ) -> cranelift_codegen::ir::Value {
        let sym = self.codegen.semantic_context.get_symbol(sym_id);
        let sym = sym.borrow();
        if sym.is_required() {
            match sym.get_name().as_str() {
                "input" => {
                    return self.get_input_file_addr();
                }
                "output" => {
                    return self.get_output_file_addr();
                }
                _ => {
                    panic!("unexpected required variable '{}'", sym.get_name());
                }
            }
        }
        let storage = *self.codegen.data_location.get(&sym_id).unwrap();
        self.get_address_of_data_location(storage)
    }

    pub fn add_symbol_to_dispose(&mut self, sym: pasko_frontend::symbol::SymbolId) {
        debug_assert!(
            !self.symbols_to_dispose.iter().any(|&s| s == sym),
            "adding twice a symbol for disposal"
        );
        self.symbols_to_dispose.push(sym);
    }

    fn dispose_var_of_type(
        &mut self,
        addr: cranelift_codegen::ir::Value,
        addr_ty: pasko_frontend::typesystem::TypeId,
    ) {
        assert!(self.codegen.type_contains_set_types(addr_ty));
        if self
            .codegen
            .semantic_context
            .type_system
            .is_set_type(addr_ty)
        {
            let pointer_type = self.codegen.pointer_type;
            // addr is the location in the stack of the variable, so we still need to do a load.
            let pointer_value = self.builder().ins().load(
                pointer_type,
                cranelift_codegen::ir::MemFlags::new(),
                addr,
                0,
            );
            self.dispose_set_variable(pointer_value);
        } else if self
            .codegen
            .semantic_context
            .type_system
            .is_array_type(addr_ty)
        {
            let index_ty = self
                .codegen
                .semantic_context
                .type_system
                .array_type_get_index_type(addr_ty);

            let lower_bound = self
                .codegen
                .semantic_context
                .type_system
                .ordinal_type_lower_bound(index_ty);
            let upper_bound = self
                .codegen
                .semantic_context
                .type_system
                .ordinal_type_upper_bound(index_ty);

            let element_ty = self
                .codegen
                .semantic_context
                .type_system
                .array_type_get_component_type(addr_ty);

            let lower_bound = self.emit_const_integer(lower_bound);
            let upper_bound = self.emit_const_integer(upper_bound);

            let size_in_bytes = self.codegen.size_in_bytes(element_ty) as i64;
            let size_in_bytes = self.emit_const_integer(size_in_bytes);

            // Emit a loop to avoid code bloat when the extent of the array is big.
            self.emit_counted_loop(lower_bound, upper_bound, |self_, idx| {
                let offset = self_.builder().ins().imul(idx, size_in_bytes);

                let element_addr = self_.builder().ins().iadd(addr, offset);
                self_.dispose_var_of_type(element_addr, element_ty);
            });
        } else if self
            .codegen
            .semantic_context
            .type_system
            .is_record_type(addr_ty)
        {
            let fields = self
                .codegen
                .semantic_context
                .type_system
                .record_type_get_fixed_fields(addr_ty);
            if self
                .codegen
                .semantic_context
                .type_system
                .record_type_get_variant_part(addr_ty)
                .is_some()
            {
                unimplemented!("Variant types");
            }
            fields.iter().for_each(|field_id| {
                let field_sym = self.codegen.semantic_context.get_symbol(*field_id);
                let field_sym = field_sym.borrow();
                let field_ty = field_sym.get_type().unwrap();

                if self.codegen.type_contains_set_types(field_ty) {
                    let offset = self.get_offset_of_field(addr_ty, *field_id);
                    let field_addr = self.add_offset_to_address(addr, offset);
                    self.dispose_var_of_type(field_addr, field_ty);
                }
            });
        } else {
            unreachable!("no type can't contain a set and not be either a set/array/struct");
        }
    }

    // Initializes objects and subobjects of set type to zero.
    fn initialize_set_data(
        &mut self,
        addr: cranelift_codegen::ir::Value,
        addr_ty: pasko_frontend::typesystem::TypeId,
    ) {
        assert!(self.codegen.type_contains_set_types(addr_ty));
        if self
            .codegen
            .semantic_context
            .type_system
            .is_set_type(addr_ty)
        {
            let null_ptr = self.emit_const_integer(0);
            self.builder()
                .ins()
                .store(cranelift_codegen::ir::MemFlags::new(), null_ptr, addr, 0);
        } else if self
            .codegen
            .semantic_context
            .type_system
            .is_array_type(addr_ty)
        {
            let index_ty = self
                .codegen
                .semantic_context
                .type_system
                .array_type_get_index_type(addr_ty);

            let lower_bound = self
                .codegen
                .semantic_context
                .type_system
                .ordinal_type_lower_bound(index_ty);
            let upper_bound = self
                .codegen
                .semantic_context
                .type_system
                .ordinal_type_upper_bound(index_ty);

            let element_ty = self
                .codegen
                .semantic_context
                .type_system
                .array_type_get_component_type(addr_ty);

            let lower_bound = self.emit_const_integer(lower_bound);
            let upper_bound = self.emit_const_integer(upper_bound);

            let size_in_bytes = self.codegen.size_in_bytes(element_ty) as i64;
            let size_in_bytes = self.emit_const_integer(size_in_bytes);

            // Emit a loop to avoid code bloat when the extent of the array is big.
            self.emit_counted_loop(lower_bound, upper_bound, |self_, idx| {
                let offset = self_.builder().ins().imul(idx, size_in_bytes);

                let element_addr = self_.builder().ins().iadd(addr, offset);
                self_.initialize_set_data(element_addr, element_ty);
            });
        } else if self
            .codegen
            .semantic_context
            .type_system
            .is_record_type(addr_ty)
        {
            let fields = self
                .codegen
                .semantic_context
                .type_system
                .record_type_get_fixed_fields(addr_ty);
            if self
                .codegen
                .semantic_context
                .type_system
                .record_type_get_variant_part(addr_ty)
                .is_some()
            {
                unimplemented!("Variant types");
            }
            fields.iter().for_each(|field_id| {
                let field_sym = self.codegen.semantic_context.get_symbol(*field_id);
                let field_sym = field_sym.borrow();
                let field_ty = field_sym.get_type().unwrap();

                if self.codegen.type_contains_set_types(field_ty) {
                    let offset = self.get_offset_of_field(addr_ty, *field_id);
                    let field_addr = self.add_offset_to_address(addr, offset);
                    self.initialize_set_data(field_addr, field_ty);
                }
            });
        } else {
            unreachable!("no type can't contain a set and not be either a set/array/struct");
        }
    }

    pub fn dispose_symbols(&mut self) {
        let symbols_to_dispose = std::mem::take(&mut self.symbols_to_dispose);

        for sym_id in symbols_to_dispose {
            let sym = self.codegen.semantic_context.get_symbol(sym_id);
            let sym = sym.borrow();
            let ty = sym.get_type().unwrap();
            let data_loc = *self.codegen.data_location.get(&sym_id).unwrap();

            let addr = self.get_address_of_data_location(data_loc);

            self.dispose_var_of_type(addr, ty);
        }
    }

    fn compute_first_argument(
        &mut self,
        args_tree: &Option<Vec<span::SpannedBox<ast::Expr>>>,
    ) -> (
        Option<cranelift_codegen::ir::Value>,
        bool,
        pasko_frontend::typesystem::TypeId,
    ) {
        let textfile_ty = self
            .codegen
            .semantic_context
            .type_system
            .get_textfile_type();
        if let Some(args) = args_tree {
            if let Some(first_arg) = args.first() {
                match first_arg.get() {
                    ast::Expr::WriteParameter(_) => (None, true, textfile_ty),
                    _ => {
                        let first_arg_type = self
                            .codegen
                            .semantic_context
                            .get_ast_type(first_arg.id())
                            .unwrap();
                        if self
                            .codegen
                            .semantic_context
                            .type_system
                            .is_file_type(first_arg_type)
                        {
                            first_arg
                                .get()
                                .walk_mut(self, first_arg.loc(), first_arg.id());
                            let is_textfile = self
                                .codegen
                                .semantic_context
                                .type_system
                                .is_textfile_type(first_arg_type);
                            (
                                Some(self.get_value(first_arg.id())),
                                is_textfile,
                                first_arg_type,
                            )
                        } else {
                            (None, true, textfile_ty)
                        }
                    }
                }
            } else {
                (None, true, textfile_ty)
            }
        } else {
            (None, true, textfile_ty)
        }
    }

    fn normalize_environment(
        &self,
        function_symbol_id: symbol::SymbolId,
    ) -> Option<Vec<(symbol::SymbolId, usize)>> {
        let function_symbol = self.codegen.semantic_context.get_symbol(function_symbol_id);
        let function_symbol = function_symbol.borrow();
        let function_symbol_scope = function_symbol.get_scope().unwrap();

        let enclosing_symbol_id = self.codegen.get_enclosing_function(function_symbol_id);
        // If this is not a nested function, no need to initialise anything.
        enclosing_symbol_id?;

        let enclosing_symbol_id = enclosing_symbol_id.unwrap();
        let enclosing_symbol = self
            .codegen
            .semantic_context
            .get_symbol(enclosing_symbol_id);
        let enclosing_symbol = enclosing_symbol.borrow();

        let mut required_environment = enclosing_symbol
            .get_required_environment()
            .iter()
            .cloned()
            .map(|sym_id| {
                let sym = self.codegen.semantic_context.get_symbol(sym_id);
                let sym = sym.borrow();
                let scope_of_sym = sym.get_scope().unwrap();
                let env_levels_up = self
                    .codegen
                    .semantic_context
                    .scope
                    .nesting_distance(function_symbol_scope, scope_of_sym);
                (sym_id, env_levels_up)
            })
            .collect::<Vec<_>>();

        // Ensure consistent ordering as this stuff is stored in a hashmap. Order them first by nesting level and then by scope_id.
        // FIXME: Do we have to use a hashmap or we could avoid repeated symbols differently?
        required_environment.sort_by(|(a_sym_id, a_nesting_level), (b_sym_id, b_nesting_level)| {
            (*a_nesting_level, *a_sym_id).cmp(&(*b_nesting_level, *b_sym_id))
        });

        Some(required_environment)
    }

    pub fn init_enclosing_environment(&mut self, function_symbol_id: symbol::SymbolId) {
        let required_environment = self.normalize_environment(function_symbol_id);

        if required_environment.is_none() {
            return;
        }
        let required_environment = required_environment.unwrap();

        let mut idx = 0;
        let mut current_nesting_level = 0;
        required_environment
            .iter()
            .for_each(|(sym_id, env_levels_up)| {
                if current_nesting_level < *env_levels_up {
                    idx = 0;
                    current_nesting_level = *env_levels_up;
                }

                let env_var_index = idx;
                idx += 1;
                // FIXME: Parameters by reference!!!!
                self.codegen.data_location.insert(
                    *sym_id,
                    DataLocation::NestedVarValue {
                        env_levels_up: *env_levels_up,
                        env_var_index,
                    },
                );
            });
    }

    fn emit_environment_for_function_reference(
        &mut self,
        function_symbol_id: symbol::SymbolId,
    ) -> cranelift_codegen::ir::Value {
        let required_environment = self.normalize_environment(function_symbol_id);
        if let Some(required_environment) = required_environment {
            let prev_environment = self
                .enclosing_environment
                .unwrap_or(self.emit_const_integer(0));

            let required_environment = required_environment
                .iter()
                .filter_map(|(sym_id, env_level)| if *env_level == 0 { Some(sym_id) } else { None })
                .cloned()
                .collect::<Vec<_>>();

            let num_items_env = 1 + required_environment.len();
            let pointer_type = self.codegen.pointer_type;

            let new_environment_ss =
                self.allocate_storage_in_stack(num_items_env as u32 * pointer_type.bytes());
            self.codegen
                .annotations
                .new_stack_slot(new_environment_ss, "[nested-environment]");

            self.builder()
                .ins()
                .stack_store(prev_environment, new_environment_ss, 0);

            required_environment
                .iter()
                .enumerate()
                .for_each(|(idx, sym_id)| {
                    let var_addr = self.get_address_of_symbol(*sym_id);
                    self.builder().ins().stack_store(
                        var_addr,
                        new_environment_ss,
                        pointer_type.bytes() as i32 * (idx + 1) as i32,
                    );
                });

            self.builder()
                .ins()
                .stack_addr(pointer_type, new_environment_ss, 0)
        } else {
            self.emit_const_integer(0)
        }
    }

    fn get_value_of_bound_identifier(&self, sym_id: SymbolId) -> Value {
        match self.codegen.data_location.get(&sym_id).unwrap() {
            DataLocation::Value(v) => *v,
            _ => {
                unreachable!("unexpected data location for a bound identifier");
            }
        }
    }

    fn get_or_create_block_for_label(&mut self, label: usize) -> cranelift_codegen::ir::Block {
        // We can't use the entry API because the borrow checker won't like it.
        if let Some(x) = self.labeled_blocks.get(&label) {
            return *x;
        }
        let new_block = self.builder().create_block();
        self.labeled_blocks.insert(label, new_block);
        new_block
    }

    fn get_address_for_variable_reference(
        &mut self,
        id: pasko_frontend::span::SpanId,
        have_to_reload: bool,
    ) -> cranelift_codegen::ir::Value {
        if let Some(sym_id) = self.codegen.semantic_context.get_ast_symbol(id) {
            if let Some(dl) = self.codegen.data_location.get(&sym_id).cloned() {
                // First check if this variable has an address. If not, get one.
                if let DataLocation::Variable(var, None) = dl {
                    // This variable does not have an address. Allocate it first.
                    let sym = self.codegen.semantic_context.get_symbol(sym_id);
                    let sym = sym.borrow();
                    let ty = sym.get_type().unwrap();
                    let stack_slot = self.allocate_storage_for_type_in_stack(ty);
                    self.codegen.annotations.new_stack_slot(
                        stack_slot,
                        &format!("[by reference argument: {}]", sym.get_name()),
                    );
                    let pointer_type = self.codegen.pointer_type;
                    let stack_slot_address =
                        self.builder().ins().stack_addr(pointer_type, stack_slot, 0);
                    // Update the location with the address.
                    self.codegen.data_location.insert(
                        sym_id,
                        DataLocation::Variable(var, Some(stack_slot_address)),
                    );
                }

                // Write to memory the current value of the variable.
                let dl = *self.codegen.data_location.get(&sym_id).unwrap();
                match dl {
                    DataLocation::Variable(var, Some(stack_slot_address)) => {
                        let sym = self.codegen.semantic_context.get_symbol(sym_id);
                        let sym = sym.borrow();
                        let ty = sym.get_type().unwrap();
                        let value = self.builder().use_var(var);
                        self.store_value_into_address(
                            stack_slot_address,
                            ty,
                            value,
                            ty,
                            true,
                            true,
                        );
                        self.set_value(id, stack_slot_address);
                        // And remember to reload it later.
                        if have_to_reload {
                            self.variables_to_reload.push(sym_id);
                        }
                        return stack_slot_address;
                    }
                    DataLocation::Variable(_var, None) => {
                        unreachable!("we should have an address");
                    }
                    _ => {}
                }
            }
        }

        // Regular variables already have address in the stack.

        self.get_value(id)
    }

    fn get_address_or_variable(&self, id: pasko_frontend::span::SpanId) -> AddressOrVariable {
        if let Some(sym_id) = self.codegen.semantic_context.get_ast_symbol(id) {
            let dl = *self.codegen.data_location.get(&sym_id).unwrap();
            // First check if this variable has an address. If not, get one.
            if let DataLocation::Variable(var, ..) = dl {
                return AddressOrVariable::Variable(var, sym_id);
            }
        }

        // Regular variables already have address in the stack.
        let addr = self.get_value(id);
        AddressOrVariable::Address(addr)
    }

    fn get_value_from_address_or_variable(
        &mut self,
        addr_or_var: &AddressOrVariable,
        addr_ty: pasko_frontend::typesystem::TypeId,
    ) -> cranelift_codegen::ir::Value {
        match addr_or_var {
            AddressOrVariable::Variable(var, sym_id) => {
                let v = self.builder().use_var(*var);
                let symbol = self.codegen.semantic_context.get_symbol(*sym_id);
                let symbol = symbol.borrow();
                self.codegen.annotations.new_value(v, symbol.get_name());
                v
            }
            AddressOrVariable::Address(addr) => self.load_value_from_address(*addr, addr_ty),
        }
    }

    fn set_value_to_address_or_variable(
        &mut self,
        addr_or_var: &AddressOrVariable,
        addr_ty: pasko_frontend::typesystem::TypeId,
        value: cranelift_codegen::ir::Value,
        value_ty: pasko_frontend::typesystem::TypeId,
    ) {
        match addr_or_var {
            AddressOrVariable::Variable(var, sym_id) => {
                self.builder().def_var(*var, value);
                let symbol = self.codegen.semantic_context.get_symbol(*sym_id);
                let symbol = symbol.borrow();
                self.codegen.annotations.new_value(value, symbol.get_name());
            }
            AddressOrVariable::Address(addr) => {
                self.store_value_into_address_for_assignment(*addr, addr_ty, value, value_ty);
            }
        }
    }

    fn get_offset_of_field(&mut self, record_type: TypeId, field_id: SymbolId) -> i64 {
        // Make sure the type has been laid out.
        self.codegen.size_in_bytes(record_type);
        let offset = {
            let cache = self.codegen.offset_cache.borrow();
            *cache.get(&field_id).unwrap()
        } as i64;

        offset
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
                "write" | "writeln" => {
                    let is_writeln = procedure_name == "writeln";
                    // Compute first argument if any.
                    let (file_argument, is_textfile, file_type) = self.compute_first_argument(&n.1);

                    // is_writeln implies is_textfile
                    assert!(!is_writeln || is_textfile);

                    let first_argument = if file_argument.is_none() { 0 } else { 1 };
                    let file_argument = file_argument.unwrap_or_else(|| self.get_output_file_val());

                    if let Some(args) = &n.1 {
                        for arg in &args[first_argument..] {
                            let (v, type_id, total_width, fract_digits): (
                                cranelift_codegen::ir::Value,
                                TypeId,
                                Option<cranelift_codegen::ir::Value>,
                                Option<cranelift_codegen::ir::Value>,
                            ) = match arg.get() {
                                ast::Expr::WriteParameter(e) => {
                                    let value = &e.0;
                                    value.get().walk_mut(self, value.loc(), value.id());
                                    let total_width = &e.1;
                                    total_width.get().walk_mut(
                                        self,
                                        total_width.loc(),
                                        total_width.id(),
                                    );

                                    let fract_digits = e.2.as_ref().map(|x| {
                                        x.get().walk_mut(self, x.loc(), x.id());
                                        self.get_value(x.id())
                                    });
                                    (
                                        self.get_value(value.id()),
                                        self.codegen
                                            .semantic_context
                                            .get_ast_type(value.id())
                                            .unwrap(),
                                        Some(self.get_value(total_width.id())),
                                        fract_digits,
                                    )
                                }
                                _ => {
                                    arg.get().walk_mut(self, arg.loc(), arg.id());
                                    (
                                        self.get_value(arg.id()),
                                        self.codegen
                                            .semantic_context
                                            .get_ast_type(arg.id())
                                            .unwrap(),
                                        None,
                                        None,
                                    )
                                }
                            };

                            if !is_textfile {
                                let component_ty = self
                                    .codegen
                                    .semantic_context
                                    .type_system
                                    .file_type_get_component_type(file_type);
                                let component_size = self.emit_const_integer(
                                    self.codegen.size_in_bytes(component_ty) as i64,
                                );
                                // file^ := expr;
                                let func_id = self
                                    .codegen
                                    .get_runtime_function(RuntimeFunctionId::BufferVarFile);
                                let func_ref = self.get_function_reference(func_id);
                                let call = self
                                    .builder()
                                    .ins()
                                    .call(func_ref, &[file_argument, component_size]);
                                let buffer_var = {
                                    let results = self.builder().inst_results(call);
                                    assert!(results.len() == 1, "Invalid number of results");
                                    results[0]
                                };
                                self.store_expr_into_address_for_assignment(
                                    buffer_var,
                                    component_ty,
                                    arg,
                                );
                                // put(file);
                                let func_id = self
                                    .codegen
                                    .get_runtime_function(RuntimeFunctionId::PutFile);
                                let func_ref = self.get_function_reference(func_id);
                                self.builder()
                                    .ins()
                                    .call(func_ref, &[file_argument, component_size]);
                            } else if self
                                .codegen
                                .semantic_context
                                .type_system
                                .is_integer_type(type_id)
                            {
                                let func_id = self
                                    .codegen
                                    .get_runtime_function(RuntimeFunctionId::WriteTextfileI64);
                                let func_ref = self.get_function_reference(func_id);
                                let total_width =
                                    total_width.unwrap_or_else(|| self.emit_const_integer(0));
                                self.builder()
                                    .ins()
                                    .call(func_ref, &[file_argument, v, total_width]);
                            } else if self
                                .codegen
                                .semantic_context
                                .type_system
                                .is_real_type(type_id)
                            {
                                let func_id = self
                                    .codegen
                                    .get_runtime_function(RuntimeFunctionId::WriteTextfileF64);
                                let func_ref = self.get_function_reference(func_id);
                                let total_width =
                                    total_width.unwrap_or_else(|| self.emit_const_integer(0));
                                let fract_digits =
                                    fract_digits.unwrap_or_else(|| self.emit_const_integer(0));
                                self.builder()
                                    .ins()
                                    .call(func_ref, &[file_argument, v, total_width, fract_digits]);
                            } else if self
                                .codegen
                                .semantic_context
                                .type_system
                                .is_string_type(type_id)
                            {
                                let func_id = self
                                    .codegen
                                    .get_runtime_function(RuntimeFunctionId::WriteTextfileStr);
                                let func_ref = self.get_function_reference(func_id);
                                let index_type = self
                                    .codegen
                                    .semantic_context
                                    .type_system
                                    .array_type_get_index_type(type_id);
                                let extent = self
                                    .codegen
                                    .semantic_context
                                    .type_system
                                    .ordinal_type_extent(index_type);
                                let number_of_chars = self.emit_const_integer(extent);
                                self.builder()
                                    .ins()
                                    .call(func_ref, &[file_argument, v, number_of_chars]);
                            } else if self
                                .codegen
                                .semantic_context
                                .type_system
                                .is_bool_type(type_id)
                            {
                                let func_id = self
                                    .codegen
                                    .get_runtime_function(RuntimeFunctionId::WriteTextfileBool);
                                let func_ref = self.get_function_reference(func_id);
                                self.builder().ins().call(func_ref, &[file_argument, v]);
                            } else if self
                                .codegen
                                .semantic_context
                                .type_system
                                .is_char_type(type_id)
                            {
                                let func_id = self
                                    .codegen
                                    .get_runtime_function(RuntimeFunctionId::WriteTextfileChar);
                                let func_ref = self.get_function_reference(func_id);
                                self.builder().ins().call(func_ref, &[file_argument, v]);
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

                    if is_writeln {
                        let func_id = self
                            .codegen
                            .get_runtime_function(RuntimeFunctionId::WriteTextfileNewline);
                        let func_ref = self.get_function_reference(func_id);
                        self.builder().ins().call(func_ref, &[file_argument]);
                    }
                }
                "read" | "readln" => {
                    let is_readln = procedure_name == "readln";
                    // Compute first argument if any.
                    let (file_argument, is_textfile, file_type) = self.compute_first_argument(&n.1);

                    // is_readln implies is_textfile
                    assert!(!is_readln || is_textfile);

                    let first_argument = if file_argument.is_none() { 0 } else { 1 };
                    let file_argument = file_argument.unwrap_or_else(|| self.get_input_file_val());
                    if let Some(args) = &n.1 {
                        for current_arg in &args[first_argument..] {
                            // Handle conversions
                            let (arg, is_implicit_conversion) = {
                                match current_arg.get() {
                                    ast::Expr::Conversion(arg) => (&arg.0, true),
                                    _ => (current_arg, false),
                                }
                            };
                            match arg.get() {
                                ast::Expr::VariableReference(expr_var) => {
                                    let var = &expr_var.0;

                                    var.get().walk_mut(self, var.loc(), var.id());
                                    let addr_or_var = self.get_address_or_variable(var.id());

                                    let var_ty = self
                                        .codegen
                                        .semantic_context
                                        .get_ast_type(var.id())
                                        .unwrap();

                                    if !is_textfile {
                                        let component_ty = self
                                            .codegen
                                            .semantic_context
                                            .type_system
                                            .file_type_get_component_type(file_type);
                                        let component_size = self.emit_const_integer(
                                            self.codegen.size_in_bytes(component_ty) as i64,
                                        );
                                        // var := file^;
                                        // Obtain address of file buffer.
                                        let func_id = self
                                            .codegen
                                            .get_runtime_function(RuntimeFunctionId::BufferVarFile);
                                        let func_ref = self.get_function_reference(func_id);
                                        let call = self
                                            .builder()
                                            .ins()
                                            .call(func_ref, &[file_argument, component_size]);
                                        let buffer_var = {
                                            let results = self.builder().inst_results(call);
                                            assert!(
                                                results.len() == 1,
                                                "Invalid number of results"
                                            );
                                            results[0]
                                        };
                                        let expr_value =
                                            self.load_value_from_address(buffer_var, component_ty);
                                        let (expr_value, component_ty) = if is_implicit_conversion {
                                            (
                                                self.emit_conversion(
                                                    var_ty,
                                                    component_ty,
                                                    expr_value,
                                                ),
                                                var_ty,
                                            )
                                        } else {
                                            assert!(self
                                                .codegen
                                                .semantic_context
                                                .type_system
                                                .same_type(var_ty, component_ty));
                                            (expr_value, component_ty)
                                        };
                                        self.set_value_to_address_or_variable(
                                            &addr_or_var,
                                            var_ty,
                                            expr_value,
                                            component_ty,
                                        );
                                        // get(file)
                                        let func_id = self
                                            .codegen
                                            .get_runtime_function(RuntimeFunctionId::GetFile);
                                        let func_ref = self.get_function_reference(func_id);
                                        self.builder()
                                            .ins()
                                            .call(func_ref, &[file_argument, component_size]);
                                    } else if self
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
                                        let (func_id, call_args) = if self
                                            .codegen
                                            .semantic_context
                                            .type_system
                                            .is_integer_type(var_ty)
                                        {
                                            (
                                                self.codegen.get_runtime_function(
                                                    RuntimeFunctionId::ReadTextfileI64,
                                                ),
                                                vec![file_argument],
                                            )
                                        } else {
                                            (
                                                self.codegen.get_runtime_function(
                                                    RuntimeFunctionId::ReadTextfileF64,
                                                ),
                                                vec![file_argument],
                                            )
                                        };
                                        let func_ref = self.get_function_reference(func_id);

                                        let call = self
                                            .builder()
                                            .ins()
                                            .call(func_ref, call_args.as_slice());
                                        let result = {
                                            let results = self.builder().inst_results(call);
                                            assert!(
                                                results.len() == 1,
                                                "Invalid number of results"
                                            );
                                            results[0]
                                        };

                                        self.set_value_to_address_or_variable(
                                            &addr_or_var,
                                            var_ty,
                                            result,
                                            var_ty,
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
                    if is_readln {
                        let func_id = self
                            .codegen
                            .get_runtime_function(RuntimeFunctionId::ReadTextfileNewline);
                        let func_ref = self.get_function_reference(func_id);
                        self.builder().ins().call(func_ref, &[file_argument]);
                    }
                }
                "new" => {
                    if let Some(args) = &n.1 {
                        assert!(args.len() == 1);
                        let arg = &args[0];
                        let (addr_or_var, pointer_ty, var_ty) = match arg.get() {
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

                                (
                                    self.get_address_or_variable(var.id()),
                                    pointer_ty,
                                    pointee_ty,
                                )
                            }
                            _ => {
                                panic!("Invalid AST at this point!");
                            }
                        };

                        let size_bytes = self.codegen.size_in_bytes(var_ty) as i64;
                        let size_bytes = self.emit_const_integer(size_bytes);

                        let func_id = self
                            .codegen
                            .get_runtime_function(RuntimeFunctionId::PointerNew);
                        let func_ref = self.get_function_reference(func_id);
                        let call = self.builder().ins().call(func_ref, &[size_bytes]);
                        let result = {
                            let results = self.builder().inst_results(call);
                            assert!(results.len() == 1, "Invalid number of results");
                            results[0]
                        };
                        self.set_value_to_address_or_variable(
                            &addr_or_var,
                            pointer_ty,
                            result,
                            pointer_ty,
                        );
                    }
                }
                "dispose" => {
                    if let Some(args) = &n.1 {
                        assert!(args.len() == 1);
                        let arg = &args[0];
                        arg.get().walk_mut(self, arg.loc(), arg.id());

                        // let pointer_ty = self
                        //     .codegen
                        //     .semantic_context
                        //     .get_ast_type(arg.id())
                        //     .unwrap();

                        let arg_value = self.get_value(arg.id());

                        let func_id = self
                            .codegen
                            .get_runtime_function(RuntimeFunctionId::PointerDispose);
                        let func_ref = self.get_function_reference(func_id);
                        self.builder().ins().call(func_ref, &[arg_value]);

                        // TODO: We could nullify the pointer here but this means getting the address
                    }
                }
                "rewrite" => {
                    if let Some(args) = &n.1 {
                        assert!(args.len() == 1);
                        let arg = &args[0];
                        arg.get().walk_mut(self, arg.loc(), arg.id());
                        let (value, is_textfile) = {
                            let ty = self
                                .codegen
                                .semantic_context
                                .get_ast_type(arg.id())
                                .unwrap();

                            (
                                self.get_value(arg.id()),
                                self.codegen
                                    .semantic_context
                                    .type_system
                                    .is_textfile_type(ty),
                            )
                        };

                        let func_id = if is_textfile {
                            self.codegen
                                .get_runtime_function(RuntimeFunctionId::RewriteTextfile)
                        } else {
                            self.codegen
                                .get_runtime_function(RuntimeFunctionId::RewriteFile)
                        };
                        let func_ref = self.get_function_reference(func_id);
                        self.builder().ins().call(func_ref, &[value]);
                    }
                }
                "reset" => {
                    if let Some(args) = &n.1 {
                        assert!(args.len() == 1);
                        let arg = &args[0];
                        arg.get().walk_mut(self, arg.loc(), arg.id());
                        let ty = self
                            .codegen
                            .semantic_context
                            .get_ast_type(arg.id())
                            .unwrap();
                        let (value, is_textfile) = {
                            (
                                self.get_value(arg.id()),
                                self.codegen
                                    .semantic_context
                                    .type_system
                                    .is_textfile_type(ty),
                            )
                        };

                        let (func_id, args) = if is_textfile {
                            (
                                self.codegen
                                    .get_runtime_function(RuntimeFunctionId::ResetTextfile),
                                vec![value],
                            )
                        } else {
                            let component_ty = self
                                .codegen
                                .semantic_context
                                .type_system
                                .file_type_get_component_type(ty);
                            let component_size = self.codegen.size_in_bytes(component_ty);
                            (
                                self.codegen
                                    .get_runtime_function(RuntimeFunctionId::ResetFile),
                                vec![value, self.emit_const_integer(component_size as i64)],
                            )
                        };
                        let func_ref = self.get_function_reference(func_id);
                        self.builder().ins().call(func_ref, args.as_slice());
                    }
                }
                "put" => {
                    if let Some(args) = &n.1 {
                        assert!(args.len() == 1);
                        let arg = &args[0];
                        arg.get().walk_mut(self, arg.loc(), arg.id());
                        let ty = self
                            .codegen
                            .semantic_context
                            .get_ast_type(arg.id())
                            .unwrap();
                        let (value, is_textfile) = {
                            (
                                self.get_value(arg.id()),
                                self.codegen
                                    .semantic_context
                                    .type_system
                                    .is_textfile_type(ty),
                            )
                        };

                        let (func_id, args) = if is_textfile {
                            (
                                self.codegen
                                    .get_runtime_function(RuntimeFunctionId::PutTextfile),
                                vec![value],
                            )
                        } else {
                            let component_ty = self
                                .codegen
                                .semantic_context
                                .type_system
                                .file_type_get_component_type(ty);
                            let component_size = self.codegen.size_in_bytes(component_ty);
                            (
                                self.codegen
                                    .get_runtime_function(RuntimeFunctionId::PutFile),
                                vec![value, self.emit_const_integer(component_size as i64)],
                            )
                        };
                        let func_ref = self.get_function_reference(func_id);
                        self.builder().ins().call(func_ref, args.as_slice());
                    }
                }
                "get" => {
                    if let Some(args) = &n.1 {
                        assert!(args.len() == 1);
                        let arg = &args[0];
                        arg.get().walk_mut(self, arg.loc(), arg.id());
                        let ty = self
                            .codegen
                            .semantic_context
                            .get_ast_type(arg.id())
                            .unwrap();
                        let (value, is_textfile) = {
                            (
                                self.get_value(arg.id()),
                                self.codegen
                                    .semantic_context
                                    .type_system
                                    .is_textfile_type(ty),
                            )
                        };

                        let (func_id, args) = if is_textfile {
                            (
                                self.codegen
                                    .get_runtime_function(RuntimeFunctionId::GetTextfile),
                                vec![value],
                            )
                        } else {
                            let component_ty = self
                                .codegen
                                .semantic_context
                                .type_system
                                .file_type_get_component_type(ty);
                            let component_size = self.codegen.size_in_bytes(component_ty);
                            (
                                self.codegen
                                    .get_runtime_function(RuntimeFunctionId::GetFile),
                                vec![value, self.emit_const_integer(component_size as i64)],
                            )
                        };
                        let func_ref = self.get_function_reference(func_id);
                        self.builder().ins().call(func_ref, args.as_slice());
                    }
                }
                _ => {
                    panic!(
                        "Lowering of call to required procedure {} not implemented yet",
                        procedure_name
                    );
                }
            }

            // Reload variables that had to be copied to memory so they can be passed by references.
            self.reload_variables();
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
            let callee_parameters = callee_sym
                .get_formal_parameters()
                .unwrap()
                .iter()
                .flatten()
                .cloned()
                .collect::<Vec<_>>();

            let args = {
                if let Some(args) = args {
                    assert!(args.len() == callee_parameters.len());
                    args.iter().zip(callee_parameters).collect::<Vec<_>>()
                } else {
                    vec![]
                }
            };

            self.emit_procedure_call(callee_sym_id, args);
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
        let v = self.emit_const_integer(*n.0.get());
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
            let v = self.emit_string_literal(n.0.get());
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

    fn visit_pre_expr_range(
        &mut self,
        _n: &ast::ExprRange,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        // This node is handled manually in the visitor of set literal
        true
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

        let num_total_members = n.0.len();
        let num_range_members =
            n.0.iter()
                .filter(|member| matches!(member.get(), ast::Expr::Range(..)))
                .count();
        let num_expr_members = num_total_members - num_range_members;

        let set_type = self.codegen.semantic_context.get_ast_type(id).unwrap();
        let element_type = self
            .codegen
            .semantic_context
            .type_system
            .set_type_get_element(set_type);
        let element_size = self.codegen.size_in_bytes(element_type);

        let call = if num_expr_members > 0 {
            // FIXME: This could blow the stack if the number of members is large.
            let stack_slot =
                self.allocate_storage_in_stack((element_size * num_expr_members) as u32);
            self.codegen
                .annotations
                .new_stack_slot(stack_slot, "[set-constructor]");
            let pointer = self.codegen.pointer_type;
            for (index, value) in n.0.iter().enumerate() {
                let value = self.get_value(value.id());
                self.builder()
                    .ins()
                    .stack_store(value, stack_slot, (element_size * index) as i32);
            }

            let number_of_elements_value = self.emit_const_integer(num_expr_members as i64);

            let func_id = self.codegen.get_runtime_function(RuntimeFunctionId::SetNew);
            let func_ref = self.get_function_reference(func_id);
            let stack_addr = self.builder().ins().stack_addr(pointer, stack_slot, 0);
            self.builder()
                .ins()
                .call(func_ref, &[number_of_elements_value, stack_addr])
        } else {
            let zero = self.emit_const_integer(0_i64);
            let func_id = self.codegen.get_runtime_function(RuntimeFunctionId::SetNew);
            let func_ref = self.get_function_reference(func_id);
            self.builder().ins().call(func_ref, &[zero, zero])
        };

        let result = {
            let results = self.builder().inst_results(call);
            assert!(results.len() == 1, "Invalid number of results");
            results[0]
        };

        let result = if num_range_members > 0 {
            // Range members will be added in a loop to avoid blowing the stack very easily.
            let set_stack_slot = self.allocate_storage_for_type_in_stack(set_type);
            self.codegen
                .annotations
                .new_stack_slot(set_stack_slot, "[set-constructor-tmp-set]");
            let pointer = self.codegen.pointer_type;
            let set_tmp_stack = self.builder().ins().stack_addr(pointer, set_stack_slot, 0);
            self.builder().ins().stack_store(result, set_stack_slot, 0);

            let element_stack_slot = self.allocate_storage_for_type_in_stack(element_type);
            self.codegen
                .annotations
                .new_stack_slot(element_stack_slot, "[set-constructor-ith-element]");
            let element_stack_addr =
                self.builder()
                    .ins()
                    .stack_addr(pointer, element_stack_slot, 0);
            let one = self.emit_const_integer(1_i64);

            n.0.iter()
                .filter(|member| matches!(member.get(), ast::Expr::Range(..)))
                .for_each(|member| {
                    match member.get() {
                        ast::Expr::Range(range) => {
                            let ast::ExprRange(lower_bound, upper_bound) = range;
                            let lower_bound_val = self.get_value(lower_bound.id());
                            let upper_bound_val = self.get_value(upper_bound.id());

                            self.emit_counted_loop(
                                lower_bound_val,
                                upper_bound_val,
                                |self_, idx| {
                                    // This is a bit silly but emit_counted_loop gives us an 0-based index for now.
                                    let current_value =
                                        self_.builder().ins().iadd(lower_bound_val, idx);
                                    // Store the value of the current member.
                                    self_.builder().ins().stack_store(
                                        current_value,
                                        element_stack_slot,
                                        0,
                                    );

                                    // Create a singleton.
                                    let func_id = self_
                                        .codegen
                                        .get_runtime_function(RuntimeFunctionId::SetNew);
                                    let func_ref = self_.get_function_reference(func_id);
                                    let call = self_
                                        .builder()
                                        .ins()
                                        .call(func_ref, &[one, element_stack_addr]);
                                    let singleton = {
                                        let results = self_.builder().inst_results(call);
                                        assert!(results.len() == 1, "Invalid number of results");
                                        results[0]
                                    };

                                    let previous_set =
                                        self_.load_value_from_address(set_tmp_stack, set_type);

                                    // Union with the current set
                                    let func_id = self_
                                        .codegen
                                        .get_runtime_function(RuntimeFunctionId::SetUnion);
                                    let func_ref = self_.get_function_reference(func_id);
                                    let call = self_
                                        .builder()
                                        .ins()
                                        .call(func_ref, &[previous_set, singleton]);
                                    let union_set = {
                                        let results = self_.builder().inst_results(call);
                                        assert!(results.len() == 1, "Invalid number of results");
                                        results[0]
                                    };
                                    // Now free the previous set and the singleton.
                                    let func_id = self_
                                        .codegen
                                        .get_runtime_function(RuntimeFunctionId::SetDispose);
                                    let func_ref = self_.get_function_reference(func_id);
                                    self_.builder().ins().call(func_ref, &[singleton]);
                                    self_.builder().ins().call(func_ref, &[previous_set]);

                                    // Update the previous set
                                    self_.builder().ins().store(
                                        cranelift_codegen::ir::MemFlags::new(),
                                        union_set,
                                        set_tmp_stack,
                                        0,
                                    );
                                },
                            );
                        }
                        _ => unreachable!(),
                    }
                });
            self.load_value_from_address(set_tmp_stack, set_type)
        } else {
            result
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

        n.0.get().walk_mut(self, n.0.loc(), n.0.id());

        // Special handling for variables.
        if let Some(sym_id) = self.codegen.semantic_context.get_ast_symbol(n.0.id()) {
            if let Some(DataLocation::Variable(var, ..)) =
                self.codegen.data_location.get(&sym_id).cloned()
            {
                let val = self.get_value(n.1.id());
                self.builder().def_var(var, val);
                let sym = self.codegen.semantic_context.get_symbol(sym_id);
                let sym = sym.borrow();
                self.codegen.annotations.new_value(val, sym.get_name());
                // And we are done.
                return false;
            }
        }

        let addr = self.get_value(n.0.id());
        let addr_ty = self
            .codegen
            .semantic_context
            .get_ast_type(n.0.id())
            .unwrap();

        self.store_expr_into_address_for_assignment(addr, addr_ty, &n.1);

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

        // Special handling of variables.
        if let Some(sym_id) = self.codegen.semantic_context.get_ast_symbol(n.0.id()) {
            let dl = self.codegen.data_location.get(&sym_id).cloned();
            if let Some(DataLocation::Variable(var, ..)) = dl {
                let v = self.builder().use_var(var);
                let symbol = self.codegen.semantic_context.get_symbol(sym_id);
                let symbol = symbol.borrow();
                self.codegen.annotations.new_value(v, symbol.get_name());
                self.set_value(id, v);
                return false;
            }
        }

        let addr = self.get_value(n.0.id());
        let ty = self
            .codegen
            .semantic_context
            .get_ast_type(n.0.id())
            .unwrap();

        let v = self.load_value_from_address(addr, ty);
        self.set_value(id, v);

        false
    }

    fn visit_pre_expr_variable_reference(
        &mut self,
        n: &ast::ExprVariableReference,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) -> bool {
        n.0.get().walk_mut(self, n.0.loc(), n.0.id());

        let addr = self.get_address_for_variable_reference(n.0.id(), true);
        self.set_value(id, addr);

        false
    }

    fn visit_assig_variable(
        &mut self,
        _n: &ast::AssigVariable,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let sym_id = self.codegen.semantic_context.get_ast_symbol(id).unwrap();
        let sym = self.codegen.semantic_context.get_symbol(sym_id);
        let sym = sym.borrow();

        // Variables do not have address.
        if sym.get_kind() == symbol::SymbolKind::Variable {
            // Some symbols like input and output don't have data location
            if let Some(DataLocation::Variable(..)) = self.codegen.data_location.get(&sym_id) {
                return;
            }
        };

        let addr_value = match sym.get_kind() {
            symbol::SymbolKind::Variable => self.get_address_of_symbol(sym_id),
            symbol::SymbolKind::Function | symbol::SymbolKind::Procedure => {
                if sym.get_parameter().is_some() {
                    self.get_address_of_symbol(sym_id)
                } else {
                    let pointer_type = self.codegen.pointer_type;
                    let func_id = *self.codegen.function_identifiers.get(&sym_id).unwrap();
                    let func_ref = self.get_function_reference(func_id);
                    self.builder().ins().func_addr(pointer_type, func_ref)
                }
            }
            symbol::SymbolKind::AssociatedField => {
                // First get the address of the associated record
                let associated_record_id = sym.associated_record().unwrap();
                let associated_field_id = sym.associated_field().unwrap();

                let associated_record_addr = self.get_value(associated_record_id);

                let record_type = self
                    .codegen
                    .semantic_context
                    .get_ast_type(associated_record_id)
                    .unwrap();

                let offset = self.get_offset_of_field(record_type, associated_field_id);

                self.add_offset_to_address(associated_record_addr, offset)
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

    fn visit_expr_bound_identifier(
        &mut self,
        _n: &ast::ExprBoundIdentifier,
        _span: &span::SpanLoc,
        id: span::SpanId,
    ) {
        let sym_id = self.codegen.semantic_context.get_ast_symbol(id).unwrap();

        let value = self.get_value_of_bound_identifier(sym_id);
        self.set_value(id, value);
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

                let lower_bound = if self
                    .codegen
                    .semantic_context
                    .type_system
                    .is_array_type(current_array_type)
                {
                    let current_array_index_type = self
                        .codegen
                        .semantic_context
                        .type_system
                        .array_type_get_index_type(current_array_type);
                    self.emit_const_integer(
                        self.codegen
                            .semantic_context
                            .type_system
                            .ordinal_type_lower_bound(current_array_index_type),
                    )
                } else if self
                    .codegen
                    .semantic_context
                    .type_system
                    .is_conformable_array_type(current_array_type)
                {
                    let sym_id = self
                        .codegen
                        .semantic_context
                        .type_system
                        .conformable_array_type_get_lower(current_array_type);
                    self.get_value_of_bound_identifier(sym_id)
                } else {
                    unreachable!("unexpected type")
                };

                let current_array_component_type = self
                    .codegen
                    .semantic_context
                    .type_system
                    .array_or_conformable_array_type_get_component_type(current_array_type);

                let (index_type_size, index_type_lb) = if current_idx + 1 == num_idx {
                    (
                        self.emit_const_integer(
                            self.codegen.size_in_bytes(current_array_component_type) as i64,
                        ),
                        lower_bound,
                    )
                } else if self
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
                    let extent = self.emit_const_integer(extent);
                    (extent, lower_bound)
                } else if self
                    .codegen
                    .semantic_context
                    .type_system
                    .is_conformable_array_type(current_array_component_type)
                {
                    let upper_bound = {
                        let sym_id = self
                            .codegen
                            .semantic_context
                            .type_system
                            .conformable_array_type_get_upper(current_array_component_type);
                        self.get_value_of_bound_identifier(sym_id)
                    };
                    let extent = self.builder().ins().isub(upper_bound, lower_bound);
                    let one = self.emit_const_integer(1);
                    let extent = self.builder().ins().iadd(extent, one);
                    (extent, lower_bound)
                } else {
                    panic!("Expecting an array type");
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

            let offset = self.builder().ins().isub(index_value, lb);
            let offset = if let Some(linear_index_bytes) = linear_index_bytes {
                self.builder().ins().iadd(index_value, linear_index_bytes)
            } else {
                offset
            };

            linear_index_bytes = Some(self.builder().ins().imul(offset, index_size));
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

        let record_type = self
            .codegen
            .semantic_context
            .get_ast_type(n.0.id())
            .unwrap();

        let field_sym = self
            .codegen
            .semantic_context
            .get_ast_symbol(n.1.id())
            .unwrap();

        let offset = self.get_offset_of_field(record_type, field_sym);

        let addr = self.add_offset_to_address(record_addr, offset);
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

        let pointee_type = self
            .codegen
            .semantic_context
            .get_ast_type(n.0.id())
            .unwrap();

        if self
            .codegen
            .semantic_context
            .type_system
            .is_file_type(pointee_type)
        {
            let is_textfile = self
                .codegen
                .semantic_context
                .type_system
                .is_textfile_type(pointee_type);
            let file_addr = self.get_value(n.0.id());
            let pointer_ty = self.codegen.pointer_type;
            let file_value = self.builder().ins().load(
                pointer_ty,
                cranelift_codegen::ir::MemFlags::new(),
                file_addr,
                0,
            );
            let component_ty = self
                .codegen
                .semantic_context
                .type_system
                .file_type_get_component_type(pointee_type);

            let (func_ref, args) = if is_textfile {
                let func_id = self
                    .codegen
                    .get_runtime_function(RuntimeFunctionId::BufferVarTextfile);
                let func_ref = self.get_function_reference(func_id);
                (func_ref, vec![file_value])
            } else {
                let component_size = self.codegen.size_in_bytes(component_ty);
                let func_id = self
                    .codegen
                    .get_runtime_function(RuntimeFunctionId::BufferVarFile);
                let func_ref = self.get_function_reference(func_id);
                let size = self.emit_const_integer(component_size as i64);
                (func_ref, vec![file_value, size])
            };

            let call = self.builder().ins().call(func_ref, args.as_slice());
            let result = {
                let results = self.builder().inst_results(call);
                assert!(results.len() == 1, "Invalid number of results");
                results[0]
            };
            self.set_value(id, result);
        } else {
            if let Some(sym_id) = self.codegen.semantic_context.get_ast_symbol(n.0.id()) {
                let sym = self.codegen.semantic_context.get_symbol(sym_id);
                let sym = sym.borrow();

                // Pointer variables hold the address in their value.
                if sym.get_kind() == symbol::SymbolKind::Variable {
                    // Some symbols like input and output don't have data loctation
                    if let Some(DataLocation::Variable(var, ..)) =
                        self.codegen.data_location.get(&sym_id).cloned()
                    {
                        let value = self.builder().use_var(var);
                        self.set_value(id, value);
                        return false;
                    }
                };
            }

            let pointer_addr = self.get_value(n.0.id());

            let pointer_ty = self.codegen.pointer_type;
            let pointer_value = self.builder().ins().load(
                pointer_ty,
                cranelift_codegen::ir::MemFlags::new(),
                pointer_addr,
                0,
            );
            self.set_value(id, pointer_value);
        }

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
                let is_integer = self.codegen.semantic_context.type_system.is_integer_type(
                    self.codegen
                        .semantic_context
                        .get_ast_type(n.1.id())
                        .unwrap(),
                );
                let result = if is_integer {
                    self.builder().ins().ineg(op_value)
                } else {
                    self.builder().ins().fneg(op_value)
                };
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
                            ast::BinOperand::Equal => self
                                .codegen
                                .get_runtime_function(RuntimeFunctionId::SetEqual),
                            ast::BinOperand::Different => self
                                .codegen
                                .get_runtime_function(RuntimeFunctionId::SetNotEqual),
                            ast::BinOperand::LowerOrEqualThan => self
                                .codegen
                                .get_runtime_function(RuntimeFunctionId::SetIsSubset),
                            ast::BinOperand::GreaterOrEqualThan => {
                                // Swap operands
                                std::mem::swap(&mut op1_value, &mut op2_value);
                                self.codegen
                                    .get_runtime_function(RuntimeFunctionId::SetIsSubset)
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
                ast::BinOperand::LogicalOr => {
                    let result = self.builder().ins().bor(lhs_value, rhs_value);
                    self.set_value(id, result);
                }
                ast::BinOperand::InSet => {
                    let func_id = self
                        .codegen
                        .get_runtime_function(RuntimeFunctionId::SetContains);
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
                ast::BinOperand::Addition => self
                    .codegen
                    .get_runtime_function(RuntimeFunctionId::SetUnion),
                ast::BinOperand::Subtraction => self
                    .codegen
                    .get_runtime_function(RuntimeFunctionId::SetDifference),
                ast::BinOperand::Multiplication => self
                    .codegen
                    .get_runtime_function(RuntimeFunctionId::SetIntersection),
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

        let value = self.emit_conversion(dest_ty, src_ty, src_value);
        self.set_value(id, value);

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

        self.emit_if_then_else(
            cond_val,
            |self_: &mut FunctionCodegenVisitor<'a, 'b, 'c>| {
                then_part
                    .get()
                    .walk_mut(self_, then_part.loc(), then_part.id());
            },
            else_part.as_ref().map(|else_part| {
                |self_: &mut FunctionCodegenVisitor<'a, 'b, 'c>| {
                    else_part
                        .get()
                        .walk_mut(self_, else_part.loc(), else_part.id());
                }
            }),
        );

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
        let addr_or_var = self.get_address_or_variable(ind_var.id());

        let ind_var_ty = self
            .codegen
            .semantic_context
            .get_ast_type(ind_var.id())
            .unwrap();

        // Initialize induction var with the value of start
        self.set_value_to_address_or_variable(&addr_or_var, ind_var_ty, start_val, ind_var_ty);

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
        let ind_var_value = self.get_value_from_address_or_variable(&addr_or_var, ind_var_ty);

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
        self.set_value_to_address_or_variable(
            &addr_or_var,
            ind_var_ty,
            next_ind_var_value,
            ind_var_ty,
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
            .trap(cranelift_codegen::ir::TrapCode::unwrap_user(1));

        self.block_stack.pop();
        self.block_stack.push(after_case);

        self.builder().switch_to_block(after_case);

        false
    }

    fn visit_pre_stmt_with(
        &mut self,
        n: &ast::StmtWith,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        let variables = &n.0;

        for variable in variables {
            // Visit them to compute their addresses.
            variable.get().walk_mut(self, variable.loc(), variable.id());
        }

        let stmt = &n.1;
        stmt.get().walk_mut(self, stmt.loc(), stmt.id());
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
                || self
                    .codegen
                    .semantic_context
                    .type_system
                    .is_pointer_type(ty)
            {
                if !sym.is_captured() {
                    self.allocate_variable(sym_id);
                } else {
                    self.allocate_value_in_stack(sym_id);
                }
            } else if self.codegen.semantic_context.type_system.is_array_type(ty)
                || self.codegen.semantic_context.type_system.is_record_type(ty)
                || self.codegen.semantic_context.type_system.is_set_type(ty)
            {
                self.allocate_value_in_stack(sym_id);
            } else {
                panic!(
                    "Unexpected type '{}' in variable declaration",
                    self.codegen.semantic_context.type_system.get_type_name(ty)
                );
            }

            if self.codegen.type_contains_set_types(ty) {
                // Initialize set_types
                let location = *self.codegen.data_location.get(&sym_id).unwrap();
                match location {
                    DataLocation::StackVarValue(stack_slot) => {
                        let pointer_type = self.codegen.pointer_type;
                        let stack_addr =
                            self.builder().ins().stack_addr(pointer_type, stack_slot, 0);
                        self.initialize_set_data(stack_addr, ty);

                        self.add_symbol_to_dispose(sym_id);
                    }
                    _ => {
                        panic!("Unexpected location");
                    }
                }
            }
        }

        false
    }

    fn visit_pre_stmt_label(
        &mut self,
        n: &ast::StmtLabel,
        _span: &span::SpanLoc,
        _id: span::SpanId,
    ) -> bool {
        // Get the target block.
        let labeled_block = self.get_or_create_block_for_label(*n.0.get());

        // Finish current block jumping to the target block.
        self.builder().ins().jump(labeled_block, &[]);
        self.block_stack.pop();

        // Start the target block.
        self.block_stack.push(labeled_block);
        self.builder().switch_to_block(labeled_block);

        // And emit the current statement as usual.
        n.1.get().walk_mut(self, n.1.loc(), n.1.id());
        false
    }

    fn visit_stmt_goto(&mut self, n: &ast::StmtGoto, _span: &span::SpanLoc, _id: span::SpanId) {
        // Get the target block.
        let labeled_block = self.get_or_create_block_for_label(*n.0.get());

        // Finish the target block jumping to the target block.
        self.builder().ins().jump(labeled_block, &[]);
        self.block_stack.pop();

        // And start a new block for the following statement.
        let next_block = self.builder().create_block();
        self.block_stack.push(next_block);
        self.builder().switch_to_block(next_block);
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
        if pasko_frontend::semantic::is_required_function(callee.get()) {
            let function_name = callee.get().as_str();
            match function_name {
                "eof" => {
                    let (file_arg, is_textfile) = match args.len() {
                        0 => (self.get_input_file_val(), true),
                        1 => (
                            self.get_value(args[0].id()),
                            self.codegen.semantic_context.type_system.is_textfile_type(
                                self.codegen
                                    .semantic_context
                                    .get_ast_type(args[0].id())
                                    .unwrap(),
                            ),
                        ),
                        _ => unreachable!(),
                    };

                    let func_id = if is_textfile {
                        self.codegen
                            .get_runtime_function(RuntimeFunctionId::EofTextfile)
                    } else {
                        self.codegen
                            .get_runtime_function(RuntimeFunctionId::EofFile)
                    };
                    let func_ref = self.get_function_reference(func_id);
                    let call = self.builder().ins().call(func_ref, &[file_arg]);

                    let result = {
                        let results = self.builder().inst_results(call);
                        assert!(results.len() == 1, "Invalid number of results");
                        results[0]
                    };
                    self.value_map.insert(id, result);
                }
                "eoln" => {
                    let file_arg = match args.len() {
                        0 => self.get_input_file_val(),
                        1 => self.get_value(args[0].id()),
                        _ => unreachable!(),
                    };

                    let func_id = self
                        .codegen
                        .get_runtime_function(RuntimeFunctionId::EolnTextfile);
                    let func_ref = self.get_function_reference(func_id);
                    let call = self.builder().ins().call(func_ref, &[file_arg]);

                    let result = {
                        let results = self.builder().inst_results(call);
                        assert!(results.len() == 1, "Invalid number of results");
                        results[0]
                    };
                    self.value_map.insert(id, result);
                }
                "abs" | "sqr" | "sin" | "cos" | "exp" | "ln" | "sqrt" | "arctan" | "trunc"
                | "round" => {
                    let is_integer_arg = self.codegen.semantic_context.type_system.is_integer_type(
                        self.codegen
                            .semantic_context
                            .get_ast_type(args[0].id())
                            .unwrap(),
                    );

                    let arg = self.get_value(args[0].id());
                    let result = match function_name {
                        "abs" if is_integer_arg => self.builder().ins().iabs(arg),
                        "sqr" if is_integer_arg => self.builder().ins().imul(arg, arg),
                        "abs" if !is_integer_arg => self.builder().ins().fabs(arg),
                        "sqr" if !is_integer_arg => self.builder().ins().fmul(arg, arg),
                        "sqrt" => self.builder().ins().sqrt(arg),
                        "trunc" => {
                            let t = self.builder().ins().trunc(arg);
                            self.builder().ins().fcvt_to_sint(I64, t)
                        }
                        "round" => {
                            let t = self.builder().ins().nearest(arg);
                            self.builder().ins().fcvt_to_sint(I64, t)
                        }
                        _ => {
                            let rt_function_id = match function_name {
                                "sin" => RuntimeFunctionId::SinF64,
                                "cos" => RuntimeFunctionId::CosF64,
                                "exp" => RuntimeFunctionId::ExpF64,
                                "ln" => RuntimeFunctionId::LnF64,
                                "arctan" => RuntimeFunctionId::ArctanF64,
                                _ => {
                                    unreachable!();
                                }
                            };
                            let func_id = self.codegen.get_runtime_function(rt_function_id);
                            let func_ref = self.get_function_reference(func_id);
                            let call = self.builder().ins().call(func_ref, &[arg]);

                            let result = {
                                let results = self.builder().inst_results(call);
                                assert!(results.len() == 1, "Invalid number of results");
                                results[0]
                            };
                            result
                        }
                    };
                    self.value_map.insert(id, result);
                }
                "ord" => {
                    let arg_ty = self
                        .codegen
                        .semantic_context
                        .get_ast_type(args[0].id())
                        .unwrap();

                    let arg = self.get_value(args[0].id());
                    if self
                        .codegen
                        .semantic_context
                        .type_system
                        .is_integer_type(arg_ty)
                        || self
                            .codegen
                            .semantic_context
                            .type_system
                            .is_enum_type(arg_ty)
                        || self
                            .codegen
                            .semantic_context
                            .type_system
                            .is_subrange_type(arg_ty)
                    {
                        // No-op
                        self.value_map.insert(id, arg);
                    } else if self
                        .codegen
                        .semantic_context
                        .type_system
                        .is_bool_type(arg_ty)
                        || self
                            .codegen
                            .semantic_context
                            .type_system
                            .is_char_type(arg_ty)
                    {
                        let x = self.builder().ins().uextend(I64, arg);
                        self.value_map.insert(id, x);
                    } else {
                        unreachable!();
                    }
                }
                "chr" => {
                    let arg = self.get_value(args[0].id());
                    let x = self.builder().ins().ireduce(I32, arg);
                    self.value_map.insert(id, x);
                }
                "succ" => {
                    let arg = self.get_value(args[0].id());
                    let x = self.builder().ins().iadd_imm(arg, 1);
                    self.value_map.insert(id, x);
                }
                "pred" => {
                    let arg = self.get_value(args[0].id());
                    let x = self.builder().ins().iadd_imm(arg, -1);
                    self.value_map.insert(id, x);
                }
                "odd" => {
                    let arg = self.get_value(args[0].id());
                    // ~x & 1 -> 1 & ~x
                    let one = self.emit_const_integer(1);
                    let x = self.builder().ins().band_not(one, arg);
                    let x = self.builder().ins().ireduce(I8, x);
                    self.value_map.insert(id, x);
                }
                _ => panic!("unimplemented required function {}", callee.get()),
            }

            // Temporaries of arguments cannot be disposed until we have returned from the function.
            self.dispose_temporaries();
        } else {
            let callee_sym_id = self
                .codegen
                .semantic_context
                .get_ast_symbol(callee.id())
                .unwrap();
            let callee_sym = self.codegen.semantic_context.get_symbol(callee_sym_id);
            let callee_sym = callee_sym.borrow();
            let callee_parameters = callee_sym
                .get_formal_parameters()
                .unwrap()
                .iter()
                .flatten()
                .cloned()
                .collect::<Vec<_>>();
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

            assert!(args.len() == callee_parameters.len());
            let args: Vec<_> = args.iter().zip(callee_parameters).collect();

            let (arg_return, return_stack_addr) = if return_type_is_simple {
                (None, None)
            } else {
                // Special treatment for the return when it is not simple.
                let return_type_size = self.codegen.size_in_bytes(callee_return_type);
                let stack_slot = self.allocate_storage_in_stack(return_type_size as u32);
                self.codegen
                    .annotations
                    .new_stack_slot(stack_slot, "[return-parameter]");
                let pointer = self.codegen.pointer_type;
                let stack_addr = self.builder().ins().stack_addr(pointer, stack_slot, 0);
                if self.codegen.type_contains_set_types(callee_return_type) {
                    self.initialize_set_data(stack_addr, callee_return_type);
                }
                (Some(stack_addr), Some(stack_addr))
            };

            let wants_return = return_type_is_simple;
            let return_value = self.emit_call(callee_sym_id, args, arg_return, wants_return);

            let result = {
                if return_type_is_simple {
                    return_value.unwrap()
                } else {
                    return_stack_addr.unwrap()
                }
            };

            self.value_map.insert(id, result);

            // Reload variables that had to be written to memory to be passed by reference.
            self.reload_variables();

            // Temporaries of arguments cannot be disposed until we have returned from the function.
            self.dispose_temporaries();

            // This function returns a temporary that may need disposing.
            // Note: because results of functions cannot be dropped, these
            // temporaries will never be disposed but moved.
            if self.codegen.type_contains_set_types(callee_return_type) {
                assert!(!return_type_is_simple);
                self.new_temporary_to_dispose(result, callee_return_type);
            }
        }

        false
    }
}

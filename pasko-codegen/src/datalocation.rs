#[derive(Debug, Clone, Copy)]
pub enum DataLocation {
    // This has a global address.
    GlobalVar(cranelift_module::DataId),
    // Simple local variables and parameters that can be mapped
    // straightforwardly to Cranelift IR types can use this. They have an
    // optional address in case it is actually needed.
    Variable(
        cranelift_frontend::Variable,
        Option<cranelift_codegen::ir::Value>,
    ),
    // This entity value is not stored directly in the stack.  Instead we have a variable
    // that contains an address to the value. Loading the value needs a use_var and then a load.
    // This is used for reference parameters and parameters passed by reference.
    VariableAddress(cranelift_frontend::Variable),
    // This entity value is stored directly in the stack. (i.e. loading the
    // value requires one load, storing a value just one store).
    // This is used for structs, arrays and sets.
    StackVarValue(cranelift_codegen::ir::entities::StackSlot),
    // FIXME: Remove this kind because VariableAddress should be enough.
    // This entity value is not stored directly in the stack.  Instead the stack
    // contains an address to the value (i.e., loading the value requires two
    // loads, storing a value requires first a load and then a store).
    //
    // This is used for parameters passed by reference (we store a pointer in
    // the stack of the calleE to the original variable) and for parameter of
    // types record and array that are passed by value (we store a
    // pointer in the calleE that points to temporary storage created in the calleR
    // for array or record value).
    StackVarAddress(cranelift_codegen::ir::entities::StackSlot),
    // Nested variables (including value parameters) will be this case.
    NestedVarValue {
        env_levels_up: usize,
        env_var_index: usize,
    },
    // Nested parameters by reference will be this case.
    NestedVarAddress {
        env_levels_up: usize,
        env_var_index: usize,
    },
    // Immutable value, without address. Used for bound identifiers of conformable arrays.
    Value(cranelift_codegen::ir::Value),
}

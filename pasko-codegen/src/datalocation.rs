#[derive(Debug, Clone, Copy)]
pub enum DataLocation {
    // This has a global address.
    GlobalVar(cranelift_module::DataId),
    // This entity value is stored directly in the stack. (i.e. loading the
    // value requires one load, storing a value just one store).
    //
    // This is used for local variables of all kinds.
    //
    // This is also used for parameters of simple type, pointers and set types
    // (= sets are opaque so their values is simply a pointer to an opaque set).
    StackVarValue(cranelift_codegen::ir::entities::StackSlot),
    // This entity value is not stored directly in the stack.  Instead the stack
    // contains an address to the value (i.e., loading the value requires two
    // loads, storing a value requires first a load and then a store).
    //
    // This is used for parameters passed by reference (we store a pointer in
    // the stack of the calleE to the original variable) and for parameter of
    // types ecord and array that are passed by value (we store a
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
}

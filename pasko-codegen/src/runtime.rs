#[derive(Default)]
pub struct RuntimeFunctions {
    pub write_str: Option<cranelift_module::FuncId>,
    pub write_i64: Option<cranelift_module::FuncId>,
    pub write_f64: Option<cranelift_module::FuncId>,
    pub write_bool: Option<cranelift_module::FuncId>,
    pub write_char: Option<cranelift_module::FuncId>,
    pub write_newline: Option<cranelift_module::FuncId>,
    pub read_i64: Option<cranelift_module::FuncId>,
    pub read_f64: Option<cranelift_module::FuncId>,
    pub read_newline: Option<cranelift_module::FuncId>,

    // Set operations
    pub set_new: Option<cranelift_module::FuncId>,
    pub set_dispose: Option<cranelift_module::FuncId>,
    pub set_copy: Option<cranelift_module::FuncId>,
    pub set_union: Option<cranelift_module::FuncId>,
    pub set_intersection: Option<cranelift_module::FuncId>,
    pub set_difference: Option<cranelift_module::FuncId>,
    pub set_contains: Option<cranelift_module::FuncId>,
    pub set_equal: Option<cranelift_module::FuncId>,
    pub set_not_equal: Option<cranelift_module::FuncId>,
    pub set_is_subset: Option<cranelift_module::FuncId>,

    // Pointer operations
    pub pointer_new: Option<cranelift_module::FuncId>,
    pub pointer_dispose: Option<cranelift_module::FuncId>,
}

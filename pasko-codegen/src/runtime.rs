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
}

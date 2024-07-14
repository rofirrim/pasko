#[derive(Default)]
pub struct RuntimeFunctions {
    // pub write_str: Option<cranelift_module::FuncId>,
    pub write_textfile_str: Option<cranelift_module::FuncId>,
    // pub write_i64: Option<cranelift_module::FuncId>,
    pub write_textfile_i64: Option<cranelift_module::FuncId>,
    // pub write_f64: Option<cranelift_module::FuncId>,
    pub write_textfile_f64: Option<cranelift_module::FuncId>,
    // pub write_bool: Option<cranelift_module::FuncId>,
    pub write_textfile_bool: Option<cranelift_module::FuncId>,
    // pub write_char: Option<cranelift_module::FuncId>,
    pub write_textfile_char: Option<cranelift_module::FuncId>,
    // pub write_newline: Option<cranelift_module::FuncId>,
    pub write_textfile_newline: Option<cranelift_module::FuncId>,
    // pub read_i64: Option<cranelift_module::FuncId>,
    pub read_textfile_i64: Option<cranelift_module::FuncId>,
    // pub read_f64: Option<cranelift_module::FuncId>,
    pub read_textfile_f64: Option<cranelift_module::FuncId>,
    // pub read_newline: Option<cranelift_module::FuncId>,
    pub read_textfile_newline: Option<cranelift_module::FuncId>,

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

    // Runtime
    pub init: Option<cranelift_module::FuncId>,
    pub finish: Option<cranelift_module::FuncId>,

    // File
    pub output_file: Option<cranelift_module::FuncId>,
    pub input_file: Option<cranelift_module::FuncId>,

    pub write_file: Option<cranelift_module::FuncId>,
    pub read_file: Option<cranelift_module::FuncId>,

    pub rewrite_file: Option<cranelift_module::FuncId>,
    pub rewrite_textfile: Option<cranelift_module::FuncId>,

    pub reset_file: Option<cranelift_module::FuncId>,
    pub reset_textfile: Option<cranelift_module::FuncId>,

    pub put_file: Option<cranelift_module::FuncId>,
    pub put_textfile: Option<cranelift_module::FuncId>,

    pub get_file: Option<cranelift_module::FuncId>,
    pub get_textfile: Option<cranelift_module::FuncId>,

    pub buffer_var_file: Option<cranelift_module::FuncId>,
    pub buffer_var_textfile: Option<cranelift_module::FuncId>,

    pub eof_file: Option<cranelift_module::FuncId>,
    pub eof_textfile: Option<cranelift_module::FuncId>,

    pub eoln_textfile: Option<cranelift_module::FuncId>,
}

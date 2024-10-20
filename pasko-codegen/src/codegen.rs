#![allow(unused_imports)]

use cranelift_codegen::settings::Configurable;
use pasko_frontend::ast;
use pasko_frontend::semantic;
use pasko_frontend::semantic::SemanticContext;
use pasko_frontend::span;
use pasko_frontend::typesystem::TypeId;
use pasko_frontend::visitor::{Visitable, VisitorMut};

use cranelift_codegen::ir::types::{F64, I32, I64};
use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::ir::{AbiParam, Function, Signature, UserFuncName};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::Module;

use cranelift_codegen::verifier::verify_function;

use cranelift_module::Linkage;
use cranelift_object; // ::{ObjectBuilder, ObjectModule};

use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

use crate::datalocation::DataLocation;
use crate::program::CodegenVisitor;

pub fn codegen(
    target: Option<String>,
    program: &span::SpannedBox<ast::Program>,
    semantic_context: &semantic::SemanticContext,
    object_filename: &PathBuf,
    ir_dump: bool,
) {
    let mut codegen_visitor = CodegenVisitor::new(target, semantic_context, ir_dump);

    program
        .get()
        .walk_mut(&mut codegen_visitor, program.loc(), program.id());

    if ir_dump {
        return;
    }

    codegen_visitor.emit_object(&object_filename.to_string_lossy());
}

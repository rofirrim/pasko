#[derive(Debug, Clone, Copy)]
pub enum DataLocation {
  // This has a global address.
  GlobalVar(cranelift_module::DataId),
  // This has an address in the stack
  StackVarValue(cranelift_codegen::ir::entities::StackSlot),
  StackVarAddress(cranelift_codegen::ir::entities::StackSlot),
  // This can be modified but does not have an address.
  // Variable(cranelift_frontend::Variable)
}

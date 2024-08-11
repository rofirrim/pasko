{
RUN: %pasko --mode=ast-dump --ast-dump-no-ids %s 2>&1 | FileCheck %s
}
program test;

procedure my_proc(x: real); forward;

procedure my_proc(x:real);
begin
   x := x + 1.0;
end;

begin
end.

{

CHECK: Program 4:1
CHECK-NEXT: ├─╴ProgramHeading 4:1 "test" []
CHECK-NEXT: └─╴ProgramBlock 6:1
CHECK-NEXT:    └─╴Block 6:1
CHECK-NEXT:       ├─╴ProcedureAndFunctionDeclarationPart 6:1
CHECK-NEXT:       │  ├─╴Procedure 6:1
CHECK-NEXT:       │  │  └─╴ProcedureForward 6:1 my_proc
CHECK-NEXT:       │  │     └─╴FormalParameterValue 6:19 ["x"]
CHECK-NEXT:       │  │        └─╴TypeIdentifier 6:22 "real" real
CHECK-NEXT:       │  └─╴Procedure 8:1
CHECK-NEXT:       │     └─╴ProcedureDefinition 8:1 my_proc
CHECK-NEXT:       │        ├─╴FormalParameterValue 8:19 ["x"]
CHECK-NEXT:       │        │  └─╴TypeIdentifier 8:21 "real" real
CHECK-NEXT:       │        └─╴Block 9:1
CHECK-NEXT:       │           └─╴Statement 9:1
CHECK-NEXT:       │              └─╴StmtCompound 9:1
CHECK-NEXT:       │                 ├─╴StmtAssignment 10:4 lhs real rhs real
CHECK-NEXT:       │                 │  ├─╴AssigVariable 10:4 "x" real
CHECK-NEXT:       │                 │  └─╴BinOp 10:9 + real
CHECK-NEXT:       │                 │     ├─╴ExprVariable 10:9 real
CHECK-NEXT:       │                 │     │  └─╴AssigVariable 10:9 "x" real
CHECK-NEXT:       │                 │     └─╴ExprConst 10:13 "real" <<no const>>
CHECK-NEXT:       │                 │        └─╴ConstReal 10:13 1.0 real
CHECK-NEXT:       │                 └─╴StmtEmpty 11:1
CHECK-NEXT:       └─╴Statement 13:1
CHECK-NEXT:          └─╴StmtCompound 13:1
CHECK-NEXT:             └─╴StmtEmpty 14:1

}

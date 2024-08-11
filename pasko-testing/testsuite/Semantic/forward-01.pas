{
RUN: %pasko --mode=ast-dump --ast-dump-no-ids %s 2>&1 | FileCheck %s
}
program test;

procedure my_proc(x: real); forward;

function increment(x: real): real; forward;
function increment;
begin
   increment := x + 1.0;
   my_proc(x);
end;

procedure my_proc;
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
CHECK-NEXT:       │  ├─╴Function 8:1
CHECK-NEXT:       │  │  └─╴FunctionForward 8:1 increment
CHECK-NEXT:       │  │     ├─╴FormalParameterValue 8:20 ["x"]
CHECK-NEXT:       │  │     │  └─╴TypeIdentifier 8:23 "real" real
CHECK-NEXT:       │  │     └─╴TypeIdentifier 8:30 "real" real
CHECK-NEXT:       │  ├─╴Function 9:1
CHECK-NEXT:       │  │  └─╴FunctionLateDefinition 9:1 increment
CHECK-NEXT:       │  │     └─╴Block 10:1
CHECK-NEXT:       │  │        └─╴Statement 10:1
CHECK-NEXT:       │  │           └─╴StmtCompound 10:1
CHECK-NEXT:       │  │              ├─╴StmtAssignment 11:4 lhs real rhs real
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 11:4 "increment" real
CHECK-NEXT:       │  │              │  └─╴BinOp 11:17 + real
CHECK-NEXT:       │  │              │     ├─╴ExprVariable 11:17 real
CHECK-NEXT:       │  │              │     │  └─╴AssigVariable 11:17 "x" real
CHECK-NEXT:       │  │              │     └─╴ExprConst 11:21 "real" <<no const>>
CHECK-NEXT:       │  │              │        └─╴ConstReal 11:21 1.0 real
CHECK-NEXT:       │  │              ├─╴StmtProcedureCall 12:4 "my_proc"
CHECK-NEXT:       │  │              │  └─╴ExprVariable 12:12 real
CHECK-NEXT:       │  │              │     └─╴AssigVariable 12:12 "x" real
CHECK-NEXT:       │  │              └─╴StmtEmpty 13:1
CHECK-NEXT:       │  └─╴Procedure 15:1
CHECK-NEXT:       │     └─╴ProcedureDefinition 15:1 my_proc
CHECK-NEXT:       │        └─╴Block 16:1
CHECK-NEXT:       │           └─╴Statement 16:1
CHECK-NEXT:       │              └─╴StmtCompound 16:1
CHECK-NEXT:       │                 ├─╴StmtAssignment 17:4 lhs real rhs real
CHECK-NEXT:       │                 │  ├─╴AssigVariable 17:4 "x" real
CHECK-NEXT:       │                 │  └─╴BinOp 17:9 + real
CHECK-NEXT:       │                 │     ├─╴ExprVariable 17:9 real
CHECK-NEXT:       │                 │     │  └─╴AssigVariable 17:9 "x" real
CHECK-NEXT:       │                 │     └─╴ExprConst 17:13 "real" <<no const>>
CHECK-NEXT:       │                 │        └─╴ConstReal 17:13 1.0 real
CHECK-NEXT:       │                 └─╴StmtEmpty 18:1
CHECK-NEXT:       └─╴Statement 20:1
CHECK-NEXT:          └─╴StmtCompound 20:1
CHECK-NEXT             └─╴StmtEmpty 21:1

}

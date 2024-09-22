{
RUN: %pasko --mode=ast-dump --ast-dump-no-ids %s 2>&1 | FileCheck %s
}

program main(output);

var
  a: array[1..10] of integer;

procedure foo2(var z: array[u0..l0 : integer] of integer);
begin
end;

procedure foo(y: array[u0..l0 : integer] of integer);
begin
  foo2(y);
end;

begin
  foo(a);
  foo2(a);
end.

{

CHECK: Program 5:1 
CHECK-NEXT: ├─╴ProgramHeading 5:1 "main" ["output"]
CHECK-NEXT: └─╴ProgramBlock 7:1 
CHECK-NEXT:    └─╴Block 7:1 
CHECK-NEXT:       ├─╴VariableDeclarationPart 7:1 
CHECK-NEXT:       │  └─╴VariableDeclaration 8:3 ["a"]
CHECK-NEXT:       │     └─╴ArrayType 8:6 (unpacked)
CHECK-NEXT:       │        ├─╴SubRangeType 8:12 
CHECK-NEXT:       │        │  ├─╴ConstInteger 8:12 1 integer
CHECK-NEXT:       │        │  └─╴ConstInteger 8:15 10 integer
CHECK-NEXT:       │        └─╴TypeIdentifier 8:22 "integer" integer
CHECK-NEXT:       ├─╴ProcedureAndFunctionDeclarationPart 10:1 
CHECK-NEXT:       │  ├─╴Procedure 10:1 
CHECK-NEXT:       │  │  └─╴ProcedureDefinition 10:1 foo2
CHECK-NEXT:       │  │     ├─╴FormalParamVariablealueConformableArray 10:16 ["z"]
CHECK-NEXT:       │  │     │  └─╴ConformableArraySchema 10:23 
CHECK-NEXT:       │  │     │     ├─╴IndexTypeSpecification 10:29 u0..l0
CHECK-NEXT:       │  │     │     │  └─╴TypeIdentifier 10:38 "integer" integer
CHECK-NEXT:       │  │     │     └─╴TypeIdentifier 10:50 "integer" integer
CHECK-NEXT:       │  │     └─╴Block 11:1 
CHECK-NEXT:       │  │        └─╴Statement 11:1 
CHECK-NEXT:       │  │           └─╴StmtCompound 11:1 
CHECK-NEXT:       │  │              └─╴StmtEmpty 12:1 
CHECK-NEXT:       │  └─╴Procedure 14:1 
CHECK-NEXT:       │     └─╴ProcedureDefinition 14:1 foo
CHECK-NEXT:       │        ├─╴FormalParameterValueConformableArray 14:15 ["y"]
CHECK-NEXT:       │        │  └─╴ConformableArraySchema 14:18 
CHECK-NEXT:       │        │     ├─╴IndexTypeSpecification 14:24 u0..l0
CHECK-NEXT:       │        │     │  └─╴TypeIdentifier 14:33 "integer" integer
CHECK-NEXT:       │        │     └─╴TypeIdentifier 14:45 "integer" integer
CHECK-NEXT:       │        └─╴Block 15:1 
CHECK-NEXT:       │           └─╴Statement 15:1 
CHECK-NEXT:       │              └─╴StmtCompound 15:1 
CHECK-NEXT:       │                 ├─╴StmtProcedureCall 16:3 "foo2"
CHECK-NEXT:       │                 │  └─╴ExprVariableReference 16:8 conformable array [u0..l0] of integer
CHECK-NEXT:       │                 │     └─╴AssigVariable 16:8 "y" conformable array [u0..l0] of integer
CHECK-NEXT:       │                 └─╴StmtEmpty 17:1 
CHECK-NEXT:       └─╴Statement 19:1 
CHECK-NEXT:          └─╴StmtCompound 19:1 
CHECK-NEXT:             ├─╴StmtProcedureCall 20:3 "foo"
CHECK-NEXT:             │  └─╴ExprVariable 20:7 array [1..10] of integer
CHECK-NEXT:             │     └─╴AssigVariable 20:7 "a" array [1..10] of integer
CHECK-NEXT:             ├─╴StmtProcedureCall 21:3 "foo2"
CHECK-NEXT:             │  └─╴ExprVariableReference 21:8 array [1..10] of integer
CHECK-NEXT:             │     └─╴AssigVariable 21:8 "a" array [1..10] of integer
CHECK-NEXT:             └─╴StmtEmpty 22:1 

}

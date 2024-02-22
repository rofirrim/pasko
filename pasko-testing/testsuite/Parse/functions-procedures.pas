{
RUN: %pasko --mode=ast-dump-pre --ast-dump-no-ids %s | FileCheck %s
}

program main;

function foo(a: integer; b: real) : boolean;
begin
  foo := true
end;

procedure bar(var a: boolean; var b: integer);
begin
end;

begin
end.

{

CHECK:      Program 5:1 
CHECK-NEXT: ├─╴ProgramHeading 5:1 "main" []
CHECK-NEXT: └─╴ProgramBlock 7:1 
CHECK-NEXT:    └─╴Block 7:1 
CHECK-NEXT:       ├─╴ProcedureAndFunctionDeclarationPart 7:1 
CHECK-NEXT:       │  ├─╴Function 7:1 
CHECK-NEXT:       │  │  └─╴FunctionDefinition 7:1 foo
CHECK-NEXT:       │  │     ├─╴FormalParameterValue 7:14 ["a"]
CHECK-NEXT:       │  │     │  └─╴TypeIdentifier 7:17 "integer" <<no-type>>
CHECK-NEXT:       │  │     ├─╴FormalParameterValue 7:26 ["b"]
CHECK-NEXT:       │  │     │  └─╴TypeIdentifier 7:29 "real" <<no-type>>
CHECK-NEXT:       │  │     ├─╴TypeIdentifier 7:37 "boolean" <<no-type>>
CHECK-NEXT:       │  │     └─╴Block 8:1 
CHECK-NEXT:       │  │        └─╴Statement 8:1 
CHECK-NEXT:       │  │           └─╴StmtCompound 8:1 
CHECK-NEXT:       │  │              └─╴StmtAssignment 9:3 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:       │  │                 ├─╴AssigVariable 9:3 "foo" <<no-type>>
CHECK-NEXT:       │  │                 └─╴ExprVariable 9:10 <<no-type>>
CHECK-NEXT:       │  │                    └─╴AssigVariable 9:10 "true" <<no-type>>
CHECK-NEXT:       │  └─╴Procedure 12:1 
CHECK-NEXT:       │     └─╴ProcedureDefinition 12:1 bar
CHECK-NEXT:       │        ├─╴FormalParameterVariable 12:15 ["a"]
CHECK-NEXT:       │        │  └─╴TypeIdentifier 12:22 "boolean" <<no-type>>
CHECK-NEXT:       │        ├─╴FormalParameterVariable 12:31 ["b"]
CHECK-NEXT:       │        │  └─╴TypeIdentifier 12:38 "integer" <<no-type>>
CHECK-NEXT:       │        └─╴Block 13:1 
CHECK-NEXT:       │           └─╴Statement 13:1 
CHECK-NEXT:       │              └─╴StmtCompound 13:1 
CHECK-NEXT:       │                 └─╴StmtEmpty 14:1 
CHECK-NEXT:       └─╴Statement 16:1 
CHECK-NEXT:          └─╴StmtCompound 16:1 
CHECK-NEXT:             └─╴StmtEmpty 17:1 

}


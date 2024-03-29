{
RUN: %pasko --mode=ast-dump --ast-dump-no-ids %s | FileCheck %s
}

program test;
type
  my_integer = integer;

var
  a : my_integer;

{ main }
begin
  a := 3;
end.

{

CHECK: Program 5:1 
CHECK-NEXT: ├─╴ProgramHeading 5:1 "test" []
CHECK-NEXT: └─╴ProgramBlock 6:1 
CHECK-NEXT:    └─╴Block 6:1 
CHECK-NEXT:       ├─╴TypeDefinitionPart 6:1 
CHECK-NEXT:       │  └─╴TypeDefinition 7:3 my_integer
CHECK-NEXT:       │     └─╴TypeIdentifier 7:16 "integer" integer
CHECK-NEXT:       ├─╴VariableDeclarationPart 9:1 
CHECK-NEXT:       │  └─╴VariableDeclaration 10:3 ["a"]
CHECK-NEXT:       │     └─╴TypeIdentifier 10:7 "my_integer" my_integer
CHECK-NEXT:       └─╴Statement 13:1 
CHECK-NEXT:          └─╴StmtCompound 13:1 
CHECK-NEXT:             ├─╴StmtAssignment 14:3 lhs my_integer rhs integer
CHECK-NEXT:             │  ├─╴AssigVariable 14:3 "a" my_integer
CHECK-NEXT:             │  └─╴ExprConst 14:8 "integer" <<no const>>
CHECK-NEXT:             │     └─╴ConstInteger 14:8 3 integer
CHECK-NEXT:             └─╴StmtEmpty 15:1 

}

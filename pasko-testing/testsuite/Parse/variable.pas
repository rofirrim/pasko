{
RUN: %pasko --mode=ast-dump-pre --ast-dump-no-ids %s | FileCheck %s
}

program main;
var
  a: integer;
  b, c : integer;
begin
end.

{

CHECK:      Program 5:1 
CHECK-NEXT: ├─╴ProgramHeading 5:1 "main" []
CHECK-NEXT: └─╴ProgramBlock 6:1 
CHECK-NEXT:    └─╴Block 6:1 
CHECK-NEXT:       ├─╴VariableDeclarationPart 6:1 
CHECK-NEXT:       │  ├─╴VariableDeclaration 7:3 ["a"]
CHECK-NEXT:       │  │  └─╴TypeIdentifier 7:6 "integer" <<no-type>>
CHECK-NEXT:       │  └─╴VariableDeclaration 8:3 ["b", "c"]
CHECK-NEXT:       │     └─╴TypeIdentifier 8:10 "integer" <<no-type>>
CHECK-NEXT:       └─╴Statement 9:1 
CHECK-NEXT:          └─╴StmtCompound 9:1 

}

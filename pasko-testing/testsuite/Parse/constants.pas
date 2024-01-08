{
RUN: %pasko --mode=ast-dump-pre --ast-dump-no-ids %s | FileCheck %s
}

program main;
const 
  a = 123;
  b = 1.23;
  yes = true;
  no = false;
begin
end.

{

CHECK:      Program 5:1 
CHECK-NEXT: ├─╴ProgramHeading 5:1 "main" []
CHECK-NEXT: └─╴ProgramBlock 6:1 
CHECK-NEXT:    └─╴Block 6:1 
CHECK-NEXT:       ├─╴ConstantDefinitionPart 6:1 
CHECK-NEXT:       │  ├─╴ConstantDefinition 7:3 a
CHECK-NEXT:       │  │  └─╴ConstInteger 7:7 123 <<no-type>>
CHECK-NEXT:       │  ├─╴ConstantDefinition 8:3 b
CHECK-NEXT:       │  │  └─╴ConstReal 8:7 1.23 <<no-type>>
CHECK-NEXT:       │  ├─╴ConstantDefinition 9:3 yes
CHECK-NEXT:       │  │  └─╴ConstNamed 9:9 "true" <<no-type>> <<no const>>
CHECK-NEXT:       │  └─╴ConstantDefinition 10:3 no
CHECK-NEXT:       │     └─╴ConstNamed 10:8 "false" <<no-type>> <<no const>>
CHECK-NEXT:       └─╴Statement 11:1 
CHECK-NEXT:          └─╴StmtCompound 11:1 


}

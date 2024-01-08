{
RUN: %pasko --mode=ast-dump-pre --ast-dump-no-ids %s | FileCheck %s
}

program main;
type
 a = integer;
 b = array [1..10] of integer;
begin
end.

{

CHECK:      Program 5:1
CHECK-NEXT: ├─╴ProgramHeading 5:1 "main" []
CHECK-NEXT: └─╴ProgramBlock 6:1
CHECK-NEXT:    └─╴Block 6:1
CHECK-NEXT:       ├─╴TypeDefinitionPart 6:1
CHECK-NEXT:       │  ├─╴TypeDefinition 7:2 a
CHECK-NEXT:       │  │  └─╴TypeIdentifier 7:6 "integer" <<no-type>>
CHECK-NEXT:       │  └─╴TypeDefinition 8:2 b
CHECK-NEXT:       │     └─╴ArrayType 8:6 (unpacked)
CHECK-NEXT:       │        ├─╴SubRangeType 8:13
CHECK-NEXT:       │        │  ├─╴ConstInteger 8:13 1 <<no-type>>
CHECK-NEXT:       │        │  └─╴ConstInteger 8:16 10 <<no-type>>
CHECK-NEXT:       │        └─╴TypeIdentifier 8:23 "integer" <<no-type>>
CHECK-NEXT:       └─╴Statement 9:1
CHECK-NEXT:          └─╴StmtCompound 9:1 


}

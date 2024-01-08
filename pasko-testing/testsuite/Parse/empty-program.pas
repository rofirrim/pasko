{
RUN: %pasko --mode=ast-dump-pre --ast-dump-no-ids %s | FileCheck %s
}

program main;
begin
end.

{

CHECK:      Program 5:1
CHECK-NEXT: ├─╴ProgramHeading 5:1 "main" []
CHECK-NEXT: └─╴ProgramBlock 6:1
CHECK-NEXT:    └─╴Block 6:1
CHECK-NEXT:       └─╴Statement 6:1
CHECK-NEXT:          └─╴StmtCompound 6:1

}

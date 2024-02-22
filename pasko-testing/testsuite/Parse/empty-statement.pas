{
RUN: %pasko --mode=ast-dump-pre --ast-dump-no-ids %s | FileCheck %s
}

{ This is not properly handled at the moment }

program main;
begin
  ;;;
end.

{
CHECK:      Program 7:1
CHECK-NEXT: ├─╴ProgramHeading 7:1 "main" []
CHECK-NEXT: └─╴ProgramBlock 8:1
CHECK-NEXT:    └─╴Block 8:1
CHECK-NEXT:       └─╴Statement 8:1
CHECK-NEXT:          └─╴StmtCompound 8:1
CHECK-NEXT:             ├─╴StmtEmpty 9:3
CHECK-NEXT:             ├─╴StmtEmpty 9:4
CHECK-NEXT:             ├─╴StmtEmpty 9:5
CHECK-NEXT:             └─╴StmtEmpty 10:1

}

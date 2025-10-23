{
RUN: %pasko --mode=ast-dump-pre --ast-dump-no-ids %s | FileCheck %s
}

program test;
var
 a: integer;
begin
 readln(a);
 case a of
  1, 3: writeln('Number one or three');
  2: writeln('Number two');
 end
end.

{

CHECK: Program 5:1 
CHECK-NEXT: ├─╴ProgramHeading 5:1 "test" []
CHECK-NEXT: └─╴ProgramBlock 6:1 
CHECK-NEXT:    └─╴Block 6:1 
CHECK-NEXT:       ├─╴VariableDeclarationPart 6:1 
CHECK-NEXT:       │  └─╴VariableDeclaration 7:2 ["a"]
CHECK-NEXT:       │     └─╴TypeIdentifier 7:5 "integer" <<no-type>>
CHECK-NEXT:       └─╴Statement 8:1 
CHECK-NEXT:          └─╴StmtCompound 8:1 
CHECK-NEXT:             ├─╴StmtProcedureCall 9:2 "readln"
CHECK-NEXT:             │  └─╴ExprVariable 9:9 <<no-type>>
CHECK-NEXT:             │     └─╴AssigVariable 9:9 "a" <<no-type>>
CHECK-NEXT:             └─╴StmtCase 10:2 
CHECK-NEXT:                ├─╴ExprVariable 10:7 <<no-type>>
CHECK-NEXT:                │  └─╴AssigVariable 10:7 "a" <<no-type>>
CHECK-NEXT:                ├─╴CaseListElement 11:3 
CHECK-NEXT:                │  ├─╴ConstInteger 11:3 1 <<no-type>>
CHECK-NEXT:                │  ├─╴ConstInteger 11:6 3 <<no-type>>
CHECK-NEXT:                │  └─╴StmtProcedureCall 11:9 "writeln"
CHECK-NEXT:                │     └─╴ExprConst 11:17 "<<no-type>>" <<no const>>
CHECK-NEXT:                │        └─╴ConstStringLiteral 11:17 "Number one or three"
CHECK-NEXT:                └─╴CaseListElement 12:3 
CHECK-NEXT:                   ├─╴ConstInteger 12:3 2 <<no-type>>
CHECK-NEXT:                   └─╴StmtProcedureCall 12:6 "writeln"
CHECK-NEXT:                      └─╴ExprConst 12:14 "<<no-type>>" <<no const>>
CHECK-NEXT:                         └─╴ConstStringLiteral 12:14 "Number two"

}

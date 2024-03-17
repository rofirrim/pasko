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
		2: writeln('Number two')
	end
end.

{

CHECK: Program 1:1 
CHECK-NEXT: ├─╴ProgramHeading 1:1 "test" []
CHECK-NEXT: └─╴ProgramBlock 2:1 
CHECK-NEXT:    └─╴Block 2:1 
CHECK-NEXT:       ├─╴VariableDeclarationPart 2:1 
CHECK-NEXT:       │  └─╴VariableDeclaration 3:2 ["a"]
CHECK-NEXT:       │     └─╴TypeIdentifier 3:5 "integer" integer
CHECK-NEXT:       └─╴Statement 4:1 
CHECK-NEXT:          └─╴StmtCompound 4:1 
CHECK-NEXT:             ├─╴StmtProcedureCall 5:2 "readln"
CHECK-NEXT:             │  └─╴ExprVariable 5:9 integer
CHECK-NEXT:             │     └─╴AssigVariable 5:9 "a" integer
CHECK-NEXT:             └─╴StmtCase 6:2 
CHECK-NEXT:                ├─╴ExprVariable 6:7 integer
CHECK-NEXT:                │  └─╴AssigVariable 6:7 "a" integer
CHECK-NEXT:                ├─╴CaseListElement 7:3 
CHECK-NEXT:                │  ├─╴ConstInteger 7:3 1 integer
CHECK-NEXT:                │  ├─╴ConstInteger 7:6 3 integer
CHECK-NEXT:                │  └─╴StmtProcedureCall 7:9 "writeln"
CHECK-NEXT:                │     └─╴ExprConst 7:17 "string of 19 characters" <<no const>>
CHECK-NEXT:                │        └─╴ConstStringLiteral 7:17 "Number one or three"
CHECK-NEXT:                └─╴CaseListElement 8:3 
CHECK-NEXT:                   ├─╴ConstInteger 8:3 2 integer
CHECK-NEXT:                   └─╴StmtProcedureCall 8:6 "writeln"
CHECK-NEXT:                      └─╴ExprConst 8:14 "string of 10 characters" <<no const>>
CHECK-NEXT:                         └─╴ConstStringLiteral 8:14 "Number two"

}

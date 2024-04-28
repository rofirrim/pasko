{
RUN: %pasko --mode=ast-dump --ast-dump-no-ids %s 2>&1 | FileCheck %s
}

program main;

type
  my_record = record
      a : integer;
      b : real;
  end;

procedure foo;
var
  r: my_record;
begin
  r.a := 1;
  r.b := 2.3;
  r.b := r.a + 1.0;
end;

begin
end.

{

CHECK:      Program 5:1
CHECK-NEXT: ├─╴ProgramHeading 5:1 "main" []
CHECK-NEXT: └─╴ProgramBlock 7:1
CHECK-NEXT:    └─╴Block 7:1
CHECK-NEXT:       ├─╴TypeDefinitionPart 7:1
CHECK-NEXT:       │  └─╴TypeDefinition 8:3 my_record
CHECK-NEXT:       │     └─╴RecordType 8:15 (packed)
CHECK-NEXT:       │        ├─╴RecordSection 9:7 a
CHECK-NEXT:       │        │  └─╴TypeIdentifier 9:11 "integer" integer
CHECK-NEXT:       │        └─╴RecordSection 10:7 b
CHECK-NEXT:       │           └─╴TypeIdentifier 10:11 "real" real
CHECK-NEXT:       ├─╴ProcedureAndFunctionDeclarationPart 13:1
CHECK-NEXT:       │  └─╴Procedure 13:1
CHECK-NEXT:       │     └─╴ProcedureDefinition 13:1 foo
CHECK-NEXT:       │        └─╴Block 14:1
CHECK-NEXT:       │           ├─╴VariableDeclarationPart 14:1
CHECK-NEXT:       │           │  └─╴VariableDeclaration 15:3 ["r"]
CHECK-NEXT:       │           │     └─╴TypeIdentifier 15:6 "my_record" my_record (an alias of record a : integer; b : real; end)
CHECK-NEXT:       │           └─╴Statement 16:1
CHECK-NEXT:       │              └─╴StmtCompound 16:1
CHECK-NEXT:       │                 ├─╴StmtAssignment 17:3 lhs integer rhs integer
CHECK-NEXT:       │                 │  ├─╴AssigArrayAccess 17:3 field:<a> integer
CHECK-NEXT:       │                 │  │  ├─╴AssigVariable 17:3 "r" my_record (an alias of record a : integer; b : real; end)
CHECK-NEXT:       │                 │  └─╴ExprConst 17:10 "integer" <<no const>>
CHECK-NEXT:       │                 │     └─╴ConstInteger 17:10 1 integer
CHECK-NEXT:       │                 ├─╴StmtAssignment 18:3 lhs real rhs real
CHECK-NEXT:       │                 │  ├─╴AssigArrayAccess 18:3 field:<b> real
CHECK-NEXT:       │                 │  │  ├─╴AssigVariable 18:3 "r" my_record (an alias of record a : integer; b : real; end)
CHECK-NEXT:       │                 │  └─╴ExprConst 18:10 "real" <<no const>>
CHECK-NEXT:       │                 │     └─╴ConstReal 18:10 2.3 real
CHECK-NEXT:       │                 ├─╴StmtAssignment 19:3 lhs real rhs real
CHECK-NEXT:       │                 │  ├─╴AssigArrayAccess 19:3 field:<b> real
CHECK-NEXT:       │                 │  │  ├─╴AssigVariable 19:3 "r" my_record (an alias of record a : integer; b : real; end)
CHECK-NEXT:       │                 │  └─╴BinOp 19:10 + real
CHECK-NEXT:       │                 │     ├─╴Conversion 19:10 real
CHECK-NEXT:       │                 │     │  └─╴ExprVariable 19:10 integer
CHECK-NEXT:       │                 │     │     └─╴AssigArrayAccess 19:10 field:<a> integer
CHECK-NEXT:       │                 │     │        ├─╴AssigVariable 19:10 "r" my_record (an alias of record a : integer; b : real; end)
CHECK-NEXT:       │                 │     └─╴ExprConst 19:16 "real" <<no const>>
CHECK-NEXT:       │                 │        └─╴ConstReal 19:16 1.0 real
CHECK-NEXT:       │                 └─╴StmtEmpty 20:1
CHECK-NEXT:       └─╴Statement 22:1
CHECK-NEXT:          └─╴StmtCompound 22:1
CHECK-NEXT:             └─╴StmtEmpty 23:1

}

{
RUN: %pasko --mode=ast-dump-pre --ast-dump-no-ids %s | FileCheck %s
}

program main;
var
  a: integer;
  b, c : integer;
  x : real;
begin
  c := a + b;
  c := a - b;
  c := a * b;
  c := a div b;
  c := a mod b;
  x := a / b;
  c := -a;

  c := a + b * c;
  c := a + (b * c);
  c := (a + b) * c;
end.

{

CHECK:      Program 5:1 
CHECK-NEXT: ├─╴ProgramHeading 5:1 "main" []
CHECK-NEXT: └─╴ProgramBlock 6:1 
CHECK-NEXT:    └─╴Block 6:1 
CHECK-NEXT:       ├─╴VariableDeclarationPart 6:1 
CHECK-NEXT:       │  ├─╴VariableDeclaration 7:3 ["a"]
CHECK-NEXT:       │  │  └─╴TypeIdentifier 7:6 "integer" <<no-type>>
CHECK-NEXT:       │  ├─╴VariableDeclaration 8:3 ["b", "c"]
CHECK-NEXT:       │  │  └─╴TypeIdentifier 8:10 "integer" <<no-type>>
CHECK-NEXT:       │  └─╴VariableDeclaration 9:3 ["x"]
CHECK-NEXT:       │     └─╴TypeIdentifier 9:7 "real" <<no-type>>
CHECK-NEXT:       └─╴Statement 10:1 
CHECK-NEXT:          └─╴StmtCompound 10:1 
CHECK-NEXT:             ├─╴StmtAssignment 11:3 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:             │  ├─╴AssigVariable 11:3 "c" <<no-type>>
CHECK-NEXT:             │  └─╴BinOp 11:8 + <<no-type>>
CHECK-NEXT:             │     ├─╴ExprVariable 11:8 <<no-type>>
CHECK-NEXT:             │     │  └─╴AssigVariable 11:8 "a" <<no-type>>
CHECK-NEXT:             │     └─╴ExprVariable 11:12 <<no-type>>
CHECK-NEXT:             │        └─╴AssigVariable 11:12 "b" <<no-type>>
CHECK-NEXT:             ├─╴StmtAssignment 12:3 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:             │  ├─╴AssigVariable 12:3 "c" <<no-type>>
CHECK-NEXT:             │  └─╴BinOp 12:8 - <<no-type>>
CHECK-NEXT:             │     ├─╴ExprVariable 12:8 <<no-type>>
CHECK-NEXT:             │     │  └─╴AssigVariable 12:8 "a" <<no-type>>
CHECK-NEXT:             │     └─╴ExprVariable 12:12 <<no-type>>
CHECK-NEXT:             │        └─╴AssigVariable 12:12 "b" <<no-type>>
CHECK-NEXT:             ├─╴StmtAssignment 13:3 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:             │  ├─╴AssigVariable 13:3 "c" <<no-type>>
CHECK-NEXT:             │  └─╴BinOp 13:8 * <<no-type>>
CHECK-NEXT:             │     ├─╴ExprVariable 13:8 <<no-type>>
CHECK-NEXT:             │     │  └─╴AssigVariable 13:8 "a" <<no-type>>
CHECK-NEXT:             │     └─╴ExprVariable 13:12 <<no-type>>
CHECK-NEXT:             │        └─╴AssigVariable 13:12 "b" <<no-type>>
CHECK-NEXT:             ├─╴StmtAssignment 14:3 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:             │  ├─╴AssigVariable 14:3 "c" <<no-type>>
CHECK-NEXT:             │  └─╴BinOp 14:8 div <<no-type>>
CHECK-NEXT:             │     ├─╴ExprVariable 14:8 <<no-type>>
CHECK-NEXT:             │     │  └─╴AssigVariable 14:8 "a" <<no-type>>
CHECK-NEXT:             │     └─╴ExprVariable 14:14 <<no-type>>
CHECK-NEXT:             │        └─╴AssigVariable 14:14 "b" <<no-type>>
CHECK-NEXT:             ├─╴StmtAssignment 15:3 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:             │  ├─╴AssigVariable 15:3 "c" <<no-type>>
CHECK-NEXT:             │  └─╴BinOp 15:8 mod <<no-type>>
CHECK-NEXT:             │     ├─╴ExprVariable 15:8 <<no-type>>
CHECK-NEXT:             │     │  └─╴AssigVariable 15:8 "a" <<no-type>>
CHECK-NEXT:             │     └─╴ExprVariable 15:14 <<no-type>>
CHECK-NEXT:             │        └─╴AssigVariable 15:14 "b" <<no-type>>
CHECK-NEXT:             ├─╴StmtAssignment 16:3 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:             │  ├─╴AssigVariable 16:3 "x" <<no-type>>
CHECK-NEXT:             │  └─╴BinOp 16:8 / <<no-type>>
CHECK-NEXT:             │     ├─╴ExprVariable 16:8 <<no-type>>
CHECK-NEXT:             │     │  └─╴AssigVariable 16:8 "a" <<no-type>>
CHECK-NEXT:             │     └─╴ExprVariable 16:12 <<no-type>>
CHECK-NEXT:             │        └─╴AssigVariable 16:12 "b" <<no-type>>
CHECK-NEXT:             ├─╴StmtAssignment 17:3 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:             │  ├─╴AssigVariable 17:3 "c" <<no-type>>
CHECK-NEXT:             │  └─╴UnOp 17:8 - <<no-type>>
CHECK-NEXT:             │     └─╴ExprVariable 17:9 <<no-type>>
CHECK-NEXT:             │        └─╴AssigVariable 17:9 "a" <<no-type>>
CHECK-NEXT:             ├─╴StmtAssignment 19:3 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:             │  ├─╴AssigVariable 19:3 "c" <<no-type>>
CHECK-NEXT:             │  └─╴BinOp 19:8 + <<no-type>>
CHECK-NEXT:             │     ├─╴ExprVariable 19:8 <<no-type>>
CHECK-NEXT:             │     │  └─╴AssigVariable 19:8 "a" <<no-type>>
CHECK-NEXT:             │     └─╴BinOp 19:12 * <<no-type>>
CHECK-NEXT:             │        ├─╴ExprVariable 19:12 <<no-type>>
CHECK-NEXT:             │        │  └─╴AssigVariable 19:12 "b" <<no-type>>
CHECK-NEXT:             │        └─╴ExprVariable 19:16 <<no-type>>
CHECK-NEXT:             │           └─╴AssigVariable 19:16 "c" <<no-type>>
CHECK-NEXT:             ├─╴StmtAssignment 20:3 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:             │  ├─╴AssigVariable 20:3 "c" <<no-type>>
CHECK-NEXT:             │  └─╴BinOp 20:8 + <<no-type>>
CHECK-NEXT:             │     ├─╴ExprVariable 20:8 <<no-type>>
CHECK-NEXT:             │     │  └─╴AssigVariable 20:8 "a" <<no-type>>
CHECK-NEXT:             │     └─╴ExprParentheses 20:12 
CHECK-NEXT:             │        └─╴BinOp 20:13 * <<no-type>>
CHECK-NEXT:             │           ├─╴ExprVariable 20:13 <<no-type>>
CHECK-NEXT:             │           │  └─╴AssigVariable 20:13 "b" <<no-type>>
CHECK-NEXT:             │           └─╴ExprVariable 20:17 <<no-type>>
CHECK-NEXT:             │              └─╴AssigVariable 20:17 "c" <<no-type>>
CHECK-NEXT:             └─╴StmtAssignment 21:3 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:                ├─╴AssigVariable 21:3 "c" <<no-type>>
CHECK-NEXT:                └─╴BinOp 21:8 * <<no-type>>
CHECK-NEXT:                   ├─╴ExprParentheses 21:8 
CHECK-NEXT:                   │  └─╴BinOp 21:9 + <<no-type>>
CHECK-NEXT:                   │     ├─╴ExprVariable 21:9 <<no-type>>
CHECK-NEXT:                   │     │  └─╴AssigVariable 21:9 "a" <<no-type>>
CHECK-NEXT:                   │     └─╴ExprVariable 21:13 <<no-type>>
CHECK-NEXT:                   │        └─╴AssigVariable 21:13 "b" <<no-type>>
CHECK-NEXT:                   └─╴ExprVariable 21:18 <<no-type>>
CHECK-NEXT:                      └─╴AssigVariable 21:18 "c" <<no-type>>

}

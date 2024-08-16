{
RUN: %pasko --mode=ast-dump --ast-dump-no-ids %s | FileCheck %s
}
program main;

var
  a, b : set of integer;
  t : boolean;

begin
  b := a + [];
  b := a * [];
  b := a - [];

  b := [] + a;
  b := [] * a;
  b := [] - a;


  t := a <= [];
  t := a >= [];
  t := [] <= a;
  t := [] >= a;
end.

{

CHECK: Program 4:1 
CHECK-NEXT: ├─╴ProgramHeading 4:1 "main" []
CHECK-NEXT: └─╴ProgramBlock 6:1 
CHECK-NEXT:    └─╴Block 6:1 
CHECK-NEXT:       ├─╴VariableDeclarationPart 6:1 
CHECK-NEXT:       │  ├─╴VariableDeclaration 7:3 ["a", "b"]
CHECK-NEXT:       │  │  └─╴SetType 7:10 (packed)
CHECK-NEXT:       │  │     └─╴TypeIdentifier 7:17 "integer" integer
CHECK-NEXT:       │  └─╴VariableDeclaration 8:3 ["t"]
CHECK-NEXT:       │     └─╴TypeIdentifier 8:7 "boolean" boolean
CHECK-NEXT:       └─╴Statement 10:1 
CHECK-NEXT:          └─╴StmtCompound 10:1 
CHECK-NEXT:             ├─╴StmtAssignment 11:3 lhs set of integer rhs set of integer
CHECK-NEXT:             │  ├─╴AssigVariable 11:3 "b" set of integer
CHECK-NEXT:             │  └─╴BinOp 11:8 + set of integer
CHECK-NEXT:             │     ├─╴ExprVariable 11:8 set of integer
CHECK-NEXT:             │     │  └─╴AssigVariable 11:8 "a" set of integer
CHECK-NEXT:             │     └─╴ExprSetLiteral 11:12 set of integer
CHECK-NEXT:             ├─╴StmtAssignment 12:3 lhs set of integer rhs set of integer
CHECK-NEXT:             │  ├─╴AssigVariable 12:3 "b" set of integer
CHECK-NEXT:             │  └─╴BinOp 12:8 * set of integer
CHECK-NEXT:             │     ├─╴ExprVariable 12:8 set of integer
CHECK-NEXT:             │     │  └─╴AssigVariable 12:8 "a" set of integer
CHECK-NEXT:             │     └─╴ExprSetLiteral 12:12 set of integer
CHECK-NEXT:             ├─╴StmtAssignment 13:3 lhs set of integer rhs set of integer
CHECK-NEXT:             │  ├─╴AssigVariable 13:3 "b" set of integer
CHECK-NEXT:             │  └─╴BinOp 13:8 - set of integer
CHECK-NEXT:             │     ├─╴ExprVariable 13:8 set of integer
CHECK-NEXT:             │     │  └─╴AssigVariable 13:8 "a" set of integer
CHECK-NEXT:             │     └─╴ExprSetLiteral 13:12 set of integer
CHECK-NEXT:             ├─╴StmtAssignment 15:3 lhs set of integer rhs set of integer
CHECK-NEXT:             │  ├─╴AssigVariable 15:3 "b" set of integer
CHECK-NEXT:             │  └─╴BinOp 15:8 + set of integer
CHECK-NEXT:             │     ├─╴ExprSetLiteral 15:8 set of integer
CHECK-NEXT:             │     └─╴ExprVariable 15:13 set of integer
CHECK-NEXT:             │        └─╴AssigVariable 15:13 "a" set of integer
CHECK-NEXT:             ├─╴StmtAssignment 16:3 lhs set of integer rhs set of integer
CHECK-NEXT:             │  ├─╴AssigVariable 16:3 "b" set of integer
CHECK-NEXT:             │  └─╴BinOp 16:8 * set of integer
CHECK-NEXT:             │     ├─╴ExprSetLiteral 16:8 set of integer
CHECK-NEXT:             │     └─╴ExprVariable 16:13 set of integer
CHECK-NEXT:             │        └─╴AssigVariable 16:13 "a" set of integer
CHECK-NEXT:             ├─╴StmtAssignment 17:3 lhs set of integer rhs set of integer
CHECK-NEXT:             │  ├─╴AssigVariable 17:3 "b" set of integer
CHECK-NEXT:             │  └─╴BinOp 17:8 - set of integer
CHECK-NEXT:             │     ├─╴ExprSetLiteral 17:8 set of integer
CHECK-NEXT:             │     └─╴ExprVariable 17:13 set of integer
CHECK-NEXT:             │        └─╴AssigVariable 17:13 "a" set of integer
CHECK-NEXT:             ├─╴StmtAssignment 20:3 lhs boolean rhs boolean
CHECK-NEXT:             │  ├─╴AssigVariable 20:3 "t" boolean
CHECK-NEXT:             │  └─╴BinOp 20:8 <= boolean
CHECK-NEXT:             │     ├─╴ExprVariable 20:8 set of integer
CHECK-NEXT:             │     │  └─╴AssigVariable 20:8 "a" set of integer
CHECK-NEXT:             │     └─╴ExprSetLiteral 20:13 set of integer
CHECK-NEXT:             ├─╴StmtAssignment 21:3 lhs boolean rhs boolean
CHECK-NEXT:             │  ├─╴AssigVariable 21:3 "t" boolean
CHECK-NEXT:             │  └─╴BinOp 21:8 >= boolean
CHECK-NEXT:             │     ├─╴ExprVariable 21:8 set of integer
CHECK-NEXT:             │     │  └─╴AssigVariable 21:8 "a" set of integer
CHECK-NEXT:             │     └─╴ExprSetLiteral 21:13 set of integer
CHECK-NEXT:             ├─╴StmtAssignment 22:3 lhs boolean rhs boolean
CHECK-NEXT:             │  ├─╴AssigVariable 22:3 "t" boolean
CHECK-NEXT:             │  └─╴BinOp 22:8 <= boolean
CHECK-NEXT:             │     ├─╴ExprSetLiteral 22:8 set of integer
CHECK-NEXT:             │     └─╴ExprVariable 22:14 set of integer
CHECK-NEXT:             │        └─╴AssigVariable 22:14 "a" set of integer
CHECK-NEXT:             ├─╴StmtAssignment 23:3 lhs boolean rhs boolean
CHECK-NEXT:             │  ├─╴AssigVariable 23:3 "t" boolean
CHECK-NEXT:             │  └─╴BinOp 23:8 >= boolean
CHECK-NEXT:             │     ├─╴ExprSetLiteral 23:8 set of integer
CHECK-NEXT:             │     └─╴ExprVariable 23:14 set of integer
CHECK-NEXT:             │        └─╴AssigVariable 23:14 "a" set of integer
CHECK-NEXT:             └─╴StmtEmpty 24:1

}

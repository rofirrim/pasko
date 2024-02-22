{
RUN: %pasko --mode=ast-dump-pre --ast-dump-no-ids %s | FileCheck %s
}

program main;

{
  
procedure AddVectors(var A, B, C : array [low..high : natural] of real);
var
  i : natural;
begin
  for i := low to high do A[i] := B[i] + C[i]
end 

}

function Sqrt (x : real) : real;
var
   old, estimate : real;
begin
   estimate := x;
   repeat
     old := estimate;
     estimate := (old + x / old) * 0.5
   until abs(estimate - old) < eps * estimate;
  { eps being a global constant }
  Sqrt := estimate
end;

begin
end.

{ 

CHECK: Program 5:1 
CHECK-NEXT: ├─╴ProgramHeading 5:1 "main" []
CHECK-NEXT: └─╴ProgramBlock 18:1 
CHECK-NEXT:    └─╴Block 18:1 
CHECK-NEXT:       ├─╴ProcedureAndFunctionDeclarationPart 18:1 
CHECK-NEXT:       │  └─╴Function 18:1 
CHECK-NEXT:       │     └─╴FunctionDefinition 18:1 sqrt
CHECK-NEXT:       │        ├─╴FormalParameterValue 18:16 ["x"]
CHECK-NEXT:       │        │  └─╴TypeIdentifier 18:20 "real" <<no-type>>
CHECK-NEXT:       │        ├─╴TypeIdentifier 18:28 "real" <<no-type>>
CHECK-NEXT:       │        └─╴Block 19:1 
CHECK-NEXT:       │           ├─╴VariableDeclarationPart 19:1 
CHECK-NEXT:       │           │  └─╴VariableDeclaration 20:4 ["old", "estimate"]
CHECK-NEXT:       │           │     └─╴TypeIdentifier 20:20 "real" <<no-type>>
CHECK-NEXT:       │           └─╴Statement 21:1 
CHECK-NEXT:       │              └─╴StmtCompound 21:1 
CHECK-NEXT:       │                 ├─╴StmtAssignment 22:4 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:       │                 │  ├─╴AssigVariable 22:4 "estimate" <<no-type>>
CHECK-NEXT:       │                 │  └─╴ExprVariable 22:16 <<no-type>>
CHECK-NEXT:       │                 │     └─╴AssigVariable 22:16 "x" <<no-type>>
CHECK-NEXT:       │                 ├─╴StmtRepeatUntil 23:4 
CHECK-NEXT:       │                 │  ├─╴StmtAssignment 24:6 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:       │                 │  │  ├─╴AssigVariable 24:6 "old" <<no-type>>
CHECK-NEXT:       │                 │  │  └─╴ExprVariable 24:13 <<no-type>>
CHECK-NEXT:       │                 │  │     └─╴AssigVariable 24:13 "estimate" <<no-type>>
CHECK-NEXT:       │                 │  ├─╴StmtAssignment 25:6 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:       │                 │  │  ├─╴AssigVariable 25:6 "estimate" <<no-type>>
CHECK-NEXT:       │                 │  │  └─╴BinOp 25:18 * <<no-type>>
CHECK-NEXT:       │                 │  │     ├─╴ExprParentheses 25:18 
CHECK-NEXT:       │                 │  │     │  └─╴BinOp 25:19 + <<no-type>>
CHECK-NEXT:       │                 │  │     │     ├─╴ExprVariable 25:19 <<no-type>>
CHECK-NEXT:       │                 │  │     │     │  └─╴AssigVariable 25:19 "old" <<no-type>>
CHECK-NEXT:       │                 │  │     │     └─╴BinOp 25:25 / <<no-type>>
CHECK-NEXT:       │                 │  │     │        ├─╴ExprVariable 25:25 <<no-type>>
CHECK-NEXT:       │                 │  │     │        │  └─╴AssigVariable 25:25 "x" <<no-type>>
CHECK-NEXT:       │                 │  │     │        └─╴ExprVariable 25:29 <<no-type>>
CHECK-NEXT:       │                 │  │     │           └─╴AssigVariable 25:29 "old" <<no-type>>
CHECK-NEXT:       │                 │  │     └─╴ExprConst 25:36 "<<no-type>>" <<no const>>
CHECK-NEXT:       │                 │  │        └─╴ConstReal 25:36 0.5 <<no-type>>
CHECK-NEXT:       │                 │  └─╴BinOp 26:10 < <<no-type>>
CHECK-NEXT:       │                 │     ├─╴ExprFunctionCall 26:10 abs
CHECK-NEXT:       │                 │     │  └─╴BinOp 26:14 - <<no-type>>
CHECK-NEXT:       │                 │     │     ├─╴ExprVariable 26:14 <<no-type>>
CHECK-NEXT:       │                 │     │     │  └─╴AssigVariable 26:14 "estimate" <<no-type>>
CHECK-NEXT:       │                 │     │     └─╴ExprVariable 26:25 <<no-type>>
CHECK-NEXT:       │                 │     │        └─╴AssigVariable 26:25 "old" <<no-type>>
CHECK-NEXT:       │                 │     └─╴BinOp 26:32 * <<no-type>>
CHECK-NEXT:       │                 │        ├─╴ExprVariable 26:32 <<no-type>>
CHECK-NEXT:       │                 │        │  └─╴AssigVariable 26:32 "eps" <<no-type>>
CHECK-NEXT:       │                 │        └─╴ExprVariable 26:38 <<no-type>>
CHECK-NEXT:       │                 │           └─╴AssigVariable 26:38 "estimate" <<no-type>>
CHECK-NEXT:       │                 └─╴StmtAssignment 28:3 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:       │                    ├─╴AssigVariable 28:3 "sqrt" <<no-type>>
CHECK-NEXT:       │                    └─╴ExprVariable 28:11 <<no-type>>
CHECK-NEXT:       │                       └─╴AssigVariable 28:11 "estimate" <<no-type>>
CHECK-NEXT:       └─╴Statement 31:1 
CHECK-NEXT:          └─╴StmtCompound 31:1 

}

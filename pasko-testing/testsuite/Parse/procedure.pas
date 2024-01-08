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

procedure bisect (function f(x : real) : real; a, b : real; var result : real);
{ This procedure attempts to find a zero of f(x) in (a,b) by
the method of bisection. It is assumed that the procedure is
called with suitable values of a and b such that
(f(a) < 0) and (f(b) >= 0)
The estimate is returned in the last parameter. }
const
  eps = 1e-10;
var
  midpoint : real;
begin
  { The invariant P is true by calling assumption }
  midpoint := a;
  while abs(a - b) > eps * abs(a) do begin
    midpoint := (a + b) / 2;
    if f(midpoint) < 0 then
      a := midpoint
    else
      b := midpoint
    { Which re-establishes the invariant:
    P = (f(a) < 0) and (f(b) >= 0)
    and reduces the interval (a,b) provided that the
    value of midpoint is distinct from both a and b. }
  end;
  { P together with the loop exit condition assures that a zero
  is contained in a small subinterval. Return the midpoint as
  the zero. }
  result := midpoint
end;

begin
end.

{ 

CHECK:      Program 5:1 
CHECK-NEXT: ├─╴ProgramHeading 5:1 "main" []
CHECK-NEXT: └─╴ProgramBlock 18:1 
CHECK-NEXT:    └─╴Block 18:1 
CHECK-NEXT:       ├─╴ProcedureAndFunctionDeclarationPart 18:1 
CHECK-NEXT:       │  └─╴Procedure 18:1 
CHECK-NEXT:       │     └─╴ProcedureDefinition 18:1 bisect
CHECK-NEXT:       │        ├─╴FormalParameterFunction 18:19 f
CHECK-NEXT:       │        │  ├─╴FormalParameterValue 18:30 ["x"]
CHECK-NEXT:       │        │  │  └─╴TypeIdentifier 18:34 "real" <<no-type>>
CHECK-NEXT:       │        │  └─╴TypeIdentifier 18:42 "real" <<no-type>>
CHECK-NEXT:       │        ├─╴FormalParameterValue 18:48 ["a", "b"]
CHECK-NEXT:       │        │  └─╴TypeIdentifier 18:55 "real" <<no-type>>
CHECK-NEXT:       │        ├─╴FormalParameterVariable 18:61 ["result"]
CHECK-NEXT:       │        │  └─╴TypeIdentifier 18:74 "real" <<no-type>>
CHECK-NEXT:       │        └─╴Block 24:1 
CHECK-NEXT:       │           ├─╴ConstantDefinitionPart 24:1 
CHECK-NEXT:       │           │  └─╴ConstantDefinition 25:3 eps
CHECK-NEXT:       │           │     └─╴ConstReal 25:9 1e-10 <<no-type>>
CHECK-NEXT:       │           ├─╴VariableDeclarationPart 26:1 
CHECK-NEXT:       │           │  └─╴VariableDeclaration 27:3 ["midpoint"]
CHECK-NEXT:       │           │     └─╴TypeIdentifier 27:14 "real" <<no-type>>
CHECK-NEXT:       │           └─╴Statement 28:1 
CHECK-NEXT:       │              └─╴StmtCompound 28:1 
CHECK-NEXT:       │                 ├─╴StmtAssignment 30:3 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:       │                 │  ├─╴AssigVariable 30:3 "midpoint" <<no-type>>
CHECK-NEXT:       │                 │  └─╴ExprVariable 30:15 <<no-type>>
CHECK-NEXT:       │                 │     └─╴AssigVariable 30:15 "a" <<no-type>>
CHECK-NEXT:       │                 ├─╴StmtWhileDo 31:3 
CHECK-NEXT:       │                 │  ├─╴BinOp 31:9 > <<no-type>>
CHECK-NEXT:       │                 │  │  ├─╴ExprFunctionCall 31:9 abs
CHECK-NEXT:       │                 │  │  │  └─╴BinOp 31:13 - <<no-type>>
CHECK-NEXT:       │                 │  │  │     ├─╴ExprVariable 31:13 <<no-type>>
CHECK-NEXT:       │                 │  │  │     │  └─╴AssigVariable 31:13 "a" <<no-type>>
CHECK-NEXT:       │                 │  │  │     └─╴ExprVariable 31:17 <<no-type>>
CHECK-NEXT:       │                 │  │  │        └─╴AssigVariable 31:17 "b" <<no-type>>
CHECK-NEXT:       │                 │  │  └─╴BinOp 31:22 * <<no-type>>
CHECK-NEXT:       │                 │  │     ├─╴ExprVariable 31:22 <<no-type>>
CHECK-NEXT:       │                 │  │     │  └─╴AssigVariable 31:22 "eps" <<no-type>>
CHECK-NEXT:       │                 │  │     └─╴ExprFunctionCall 31:28 abs
CHECK-NEXT:       │                 │  │        └─╴ExprVariable 31:32 <<no-type>>
CHECK-NEXT:       │                 │  │           └─╴AssigVariable 31:32 "a" <<no-type>>
CHECK-NEXT:       │                 │  └─╴StmtCompound 31:38 
CHECK-NEXT:       │                 │     ├─╴StmtAssignment 32:5 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:       │                 │     │  ├─╴AssigVariable 32:5 "midpoint" <<no-type>>
CHECK-NEXT:       │                 │     │  └─╴BinOp 32:17 / <<no-type>>
CHECK-NEXT:       │                 │     │     ├─╴ExprParentheses 32:17 
CHECK-NEXT:       │                 │     │     │  └─╴BinOp 32:18 + <<no-type>>
CHECK-NEXT:       │                 │     │     │     ├─╴ExprVariable 32:18 <<no-type>>
CHECK-NEXT:       │                 │     │     │     │  └─╴AssigVariable 32:18 "a" <<no-type>>
CHECK-NEXT:       │                 │     │     │     └─╴ExprVariable 32:22 <<no-type>>
CHECK-NEXT:       │                 │     │     │        └─╴AssigVariable 32:22 "b" <<no-type>>
CHECK-NEXT:       │                 │     │     └─╴ExprConst 32:27 "<<no-type>>" <<no const>>
CHECK-NEXT:       │                 │     │        └─╴ConstInteger 32:27 2 <<no-type>>
CHECK-NEXT:       │                 │     └─╴StmtIf 33:5 
CHECK-NEXT:       │                 │        ├─╴BinOp 33:8 < <<no-type>>
CHECK-NEXT:       │                 │        │  ├─╴ExprFunctionCall 33:8 f
CHECK-NEXT:       │                 │        │  │  └─╴ExprVariable 33:10 <<no-type>>
CHECK-NEXT:       │                 │        │  │     └─╴AssigVariable 33:10 "midpoint" <<no-type>>
CHECK-NEXT:       │                 │        │  └─╴ExprConst 33:22 "<<no-type>>" <<no const>>
CHECK-NEXT:       │                 │        │     └─╴ConstInteger 33:22 0 <<no-type>>
CHECK-NEXT:       │                 │        ├─╴StmtAssignment 34:7 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:       │                 │        │  ├─╴AssigVariable 34:7 "a" <<no-type>>
CHECK-NEXT:       │                 │        │  └─╴ExprVariable 34:12 <<no-type>>
CHECK-NEXT:       │                 │        │     └─╴AssigVariable 34:12 "midpoint" <<no-type>>
CHECK-NEXT:       │                 │        └─╴StmtAssignment 36:7 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:       │                 │           ├─╴AssigVariable 36:7 "b" <<no-type>>
CHECK-NEXT:       │                 │           └─╴ExprVariable 36:12 <<no-type>>
CHECK-NEXT:       │                 │              └─╴AssigVariable 36:12 "midpoint" <<no-type>>
CHECK-NEXT:       │                 └─╴StmtAssignment 45:3 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:       │                    ├─╴AssigVariable 45:3 "result" <<no-type>>
CHECK-NEXT:       │                    └─╴ExprVariable 45:13 <<no-type>>
CHECK-NEXT:       │                       └─╴AssigVariable 45:13 "midpoint" <<no-type>>
CHECK-NEXT:       └─╴Statement 48:1 
CHECK-NEXT:          └─╴StmtCompound 48:1 

}

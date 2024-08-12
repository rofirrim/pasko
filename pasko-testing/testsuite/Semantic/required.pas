{
RUN: %pasko --mode=ast-dump --ast-dump-no-ids %s 2>&1 | FileCheck %s
}

program test;

type
   my_enum = (red, blue, green);

procedure math(n : integer; x : real);
begin
  n := abs(n);
  n := sqr(n);

  x := sin(n);
  x := cos(n);
  x := exp(n);
  x := ln(n);
  x := sqrt(n);
  x := arctan(n);

  x := abs(x);
  x := sqr(x);
  x := sin(x);
  x := cos(x);
  x := exp(x);
  x := ln(x);
  x := sqrt(x);
  x := arctan(x);
end;

procedure transfer(x: real);
var
  n: integer;
begin
  n := trunc(x);
  n := round(x);
end;

procedure ordinals(e: my_enum; n: integer);
var
  c : char;
begin
  n := ord(e);
  c := chr(n);
  e := succ(e);
  e := pred(e);
end;

procedure bool_funcs(n: integer);
var
  b : boolean;
begin
  b := odd(n);
end;

begin
end.

{

CHECK: Program 5:1
CHECK-NEXT: ├─╴ProgramHeading 5:1 "test" []
CHECK-NEXT: └─╴ProgramBlock 7:1
CHECK-NEXT:    └─╴Block 7:1
CHECK-NEXT:       ├─╴TypeDefinitionPart 7:1
CHECK-NEXT:       │  └─╴TypeDefinition 8:4 my_enum
CHECK-NEXT:       │     └─╴EnumeratedType 8:14 red, blue, green
CHECK-NEXT:       ├─╴ProcedureAndFunctionDeclarationPart 10:1
CHECK-NEXT:       │  ├─╴Procedure 10:1
CHECK-NEXT:       │  │  └─╴ProcedureDefinition 10:1 math
CHECK-NEXT:       │  │     ├─╴FormalParameterValue 10:16 ["n"]
CHECK-NEXT:       │  │     │  └─╴TypeIdentifier 10:20 "integer" integer
CHECK-NEXT:       │  │     ├─╴FormalParameterValue 10:29 ["x"]
CHECK-NEXT:       │  │     │  └─╴TypeIdentifier 10:33 "real" real
CHECK-NEXT:       │  │     └─╴Block 11:1
CHECK-NEXT:       │  │        └─╴Statement 11:1
CHECK-NEXT:       │  │           └─╴StmtCompound 11:1
CHECK-NEXT:       │  │              ├─╴StmtAssignment 12:3 lhs integer rhs integer
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 12:3 "n" integer
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 12:8 abs integer
CHECK-NEXT:       │  │              │     └─╴ExprVariable 12:12 integer
CHECK-NEXT:       │  │              │        └─╴AssigVariable 12:12 "n" integer
CHECK-NEXT:       │  │              ├─╴StmtAssignment 13:3 lhs integer rhs integer
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 13:3 "n" integer
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 13:8 sqr integer
CHECK-NEXT:       │  │              │     └─╴ExprVariable 13:12 integer
CHECK-NEXT:       │  │              │        └─╴AssigVariable 13:12 "n" integer
CHECK-NEXT:       │  │              ├─╴StmtAssignment 15:3 lhs real rhs real
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 15:3 "x" real
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 15:8 sin real
CHECK-NEXT:       │  │              │     └─╴Conversion 15:12 real
CHECK-NEXT:       │  │              │        └─╴ExprVariable 15:12 integer
CHECK-NEXT:       │  │              │           └─╴AssigVariable 15:12 "n" integer
CHECK-NEXT:       │  │              ├─╴StmtAssignment 16:3 lhs real rhs real
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 16:3 "x" real
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 16:8 cos real
CHECK-NEXT:       │  │              │     └─╴Conversion 16:12 real
CHECK-NEXT:       │  │              │        └─╴ExprVariable 16:12 integer
CHECK-NEXT:       │  │              │           └─╴AssigVariable 16:12 "n" integer
CHECK-NEXT:       │  │              ├─╴StmtAssignment 17:3 lhs real rhs real
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 17:3 "x" real
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 17:8 exp real
CHECK-NEXT:       │  │              │     └─╴Conversion 17:12 real
CHECK-NEXT:       │  │              │        └─╴ExprVariable 17:12 integer
CHECK-NEXT:       │  │              │           └─╴AssigVariable 17:12 "n" integer
CHECK-NEXT:       │  │              ├─╴StmtAssignment 18:3 lhs real rhs real
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 18:3 "x" real
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 18:8 ln real
CHECK-NEXT:       │  │              │     └─╴Conversion 18:11 real
CHECK-NEXT:       │  │              │        └─╴ExprVariable 18:11 integer
CHECK-NEXT:       │  │              │           └─╴AssigVariable 18:11 "n" integer
CHECK-NEXT:       │  │              ├─╴StmtAssignment 19:3 lhs real rhs real
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 19:3 "x" real
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 19:8 sqrt real
CHECK-NEXT:       │  │              │     └─╴Conversion 19:13 real
CHECK-NEXT:       │  │              │        └─╴ExprVariable 19:13 integer
CHECK-NEXT:       │  │              │           └─╴AssigVariable 19:13 "n" integer
CHECK-NEXT:       │  │              ├─╴StmtAssignment 20:3 lhs real rhs real
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 20:3 "x" real
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 20:8 arctan real
CHECK-NEXT:       │  │              │     └─╴Conversion 20:15 real
CHECK-NEXT:       │  │              │        └─╴ExprVariable 20:15 integer
CHECK-NEXT:       │  │              │           └─╴AssigVariable 20:15 "n" integer
CHECK-NEXT:       │  │              ├─╴StmtAssignment 22:3 lhs real rhs real
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 22:3 "x" real
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 22:8 abs real
CHECK-NEXT:       │  │              │     └─╴ExprVariable 22:12 real
CHECK-NEXT:       │  │              │        └─╴AssigVariable 22:12 "x" real
CHECK-NEXT:       │  │              ├─╴StmtAssignment 23:3 lhs real rhs real
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 23:3 "x" real
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 23:8 sqr real
CHECK-NEXT:       │  │              │     └─╴ExprVariable 23:12 real
CHECK-NEXT:       │  │              │        └─╴AssigVariable 23:12 "x" real
CHECK-NEXT:       │  │              ├─╴StmtAssignment 24:3 lhs real rhs real
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 24:3 "x" real
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 24:8 sin real
CHECK-NEXT:       │  │              │     └─╴ExprVariable 24:12 real
CHECK-NEXT:       │  │              │        └─╴AssigVariable 24:12 "x" real
CHECK-NEXT:       │  │              ├─╴StmtAssignment 25:3 lhs real rhs real
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 25:3 "x" real
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 25:8 cos real
CHECK-NEXT:       │  │              │     └─╴ExprVariable 25:12 real
CHECK-NEXT:       │  │              │        └─╴AssigVariable 25:12 "x" real
CHECK-NEXT:       │  │              ├─╴StmtAssignment 26:3 lhs real rhs real
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 26:3 "x" real
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 26:8 exp real
CHECK-NEXT:       │  │              │     └─╴ExprVariable 26:12 real
CHECK-NEXT:       │  │              │        └─╴AssigVariable 26:12 "x" real
CHECK-NEXT:       │  │              ├─╴StmtAssignment 27:3 lhs real rhs real
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 27:3 "x" real
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 27:8 ln real
CHECK-NEXT:       │  │              │     └─╴ExprVariable 27:11 real
CHECK-NEXT:       │  │              │        └─╴AssigVariable 27:11 "x" real
CHECK-NEXT:       │  │              ├─╴StmtAssignment 28:3 lhs real rhs real
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 28:3 "x" real
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 28:8 sqrt real
CHECK-NEXT:       │  │              │     └─╴ExprVariable 28:13 real
CHECK-NEXT:       │  │              │        └─╴AssigVariable 28:13 "x" real
CHECK-NEXT:       │  │              ├─╴StmtAssignment 29:3 lhs real rhs real
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 29:3 "x" real
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 29:8 arctan real
CHECK-NEXT:       │  │              │     └─╴ExprVariable 29:15 real
CHECK-NEXT:       │  │              │        └─╴AssigVariable 29:15 "x" real
CHECK-NEXT:       │  │              └─╴StmtEmpty 30:1
CHECK-NEXT:       │  ├─╴Procedure 32:1
CHECK-NEXT:       │  │  └─╴ProcedureDefinition 32:1 transfer
CHECK-NEXT:       │  │     ├─╴FormalParameterValue 32:20 ["x"]
CHECK-NEXT:       │  │     │  └─╴TypeIdentifier 32:23 "real" real
CHECK-NEXT:       │  │     └─╴Block 33:1
CHECK-NEXT:       │  │        ├─╴VariableDeclarationPart 33:1
CHECK-NEXT:       │  │        │  └─╴VariableDeclaration 34:3 ["n"]
CHECK-NEXT:       │  │        │     └─╴TypeIdentifier 34:6 "integer" integer
CHECK-NEXT:       │  │        └─╴Statement 35:1
CHECK-NEXT:       │  │           └─╴StmtCompound 35:1
CHECK-NEXT:       │  │              ├─╴StmtAssignment 36:3 lhs integer rhs integer
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 36:3 "n" integer
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 36:8 trunc integer
CHECK-NEXT:       │  │              │     └─╴ExprVariable 36:14 real
CHECK-NEXT:       │  │              │        └─╴AssigVariable 36:14 "x" real
CHECK-NEXT:       │  │              ├─╴StmtAssignment 37:3 lhs integer rhs integer
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 37:3 "n" integer
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 37:8 round integer
CHECK-NEXT:       │  │              │     └─╴ExprVariable 37:14 real
CHECK-NEXT:       │  │              │        └─╴AssigVariable 37:14 "x" real
CHECK-NEXT:       │  │              └─╴StmtEmpty 38:1
CHECK-NEXT:       │  ├─╴Procedure 40:1
CHECK-NEXT:       │  │  └─╴ProcedureDefinition 40:1 ordinals
CHECK-NEXT:       │  │     ├─╴FormalParameterValue 40:20 ["e"]
CHECK-NEXT:       │  │     │  └─╴TypeIdentifier 40:23 "my_enum" my_enum (an alias of (red, blue, green))
CHECK-NEXT:       │  │     ├─╴FormalParameterValue 40:32 ["n"]
CHECK-NEXT:       │  │     │  └─╴TypeIdentifier 40:35 "integer" integer
CHECK-NEXT:       │  │     └─╴Block 41:1
CHECK-NEXT:       │  │        ├─╴VariableDeclarationPart 41:1
CHECK-NEXT:       │  │        │  └─╴VariableDeclaration 42:3 ["c"]
CHECK-NEXT:       │  │        │     └─╴TypeIdentifier 42:7 "char" char
CHECK-NEXT:       │  │        └─╴Statement 43:1
CHECK-NEXT:       │  │           └─╴StmtCompound 43:1
CHECK-NEXT:       │  │              ├─╴StmtAssignment 44:3 lhs integer rhs integer
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 44:3 "n" integer
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 44:8 ord integer
CHECK-NEXT:       │  │              │     └─╴ExprVariable 44:12 my_enum (an alias of (red, blue, green))
CHECK-NEXT:       │  │              │        └─╴AssigVariable 44:12 "e" my_enum (an alias of (red, blue, green))
CHECK-NEXT:       │  │              ├─╴StmtAssignment 45:3 lhs char rhs char
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 45:3 "c" char
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 45:8 chr char
CHECK-NEXT:       │  │              │     └─╴ExprVariable 45:12 integer
CHECK-NEXT:       │  │              │        └─╴AssigVariable 45:12 "n" integer
CHECK-NEXT:       │  │              ├─╴StmtAssignment 46:3 lhs my_enum (an alias of (red, blue, green)) rhs my_enum (an alias of (red, blue, green))
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 46:3 "e" my_enum (an alias of (red, blue, green))
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 46:8 succ my_enum (an alias of (red, blue, green))
CHECK-NEXT:       │  │              │     └─╴ExprVariable 46:13 my_enum (an alias of (red, blue, green))
CHECK-NEXT:       │  │              │        └─╴AssigVariable 46:13 "e" my_enum (an alias of (red, blue, green))
CHECK-NEXT:       │  │              ├─╴StmtAssignment 47:3 lhs my_enum (an alias of (red, blue, green)) rhs my_enum (an alias of (red, blue, green))
CHECK-NEXT:       │  │              │  ├─╴AssigVariable 47:3 "e" my_enum (an alias of (red, blue, green))
CHECK-NEXT:       │  │              │  └─╴ExprFunctionCall 47:8 pred my_enum (an alias of (red, blue, green))
CHECK-NEXT:       │  │              │     └─╴ExprVariable 47:13 my_enum (an alias of (red, blue, green))
CHECK-NEXT:       │  │              │        └─╴AssigVariable 47:13 "e" my_enum (an alias of (red, blue, green))
CHECK-NEXT:       │  │              └─╴StmtEmpty 48:1
CHECK-NEXT:       │  └─╴Procedure 50:1
CHECK-NEXT:       │     └─╴ProcedureDefinition 50:1 bool_funcs
CHECK-NEXT:       │        ├─╴FormalParameterValue 50:22 ["n"]
CHECK-NEXT:       │        │  └─╴TypeIdentifier 50:25 "integer" integer
CHECK-NEXT:       │        └─╴Block 51:1
CHECK-NEXT:       │           ├─╴VariableDeclarationPart 51:1
CHECK-NEXT:       │           │  └─╴VariableDeclaration 52:3 ["b"]
CHECK-NEXT:       │           │     └─╴TypeIdentifier 52:7 "boolean" boolean
CHECK-NEXT:       │           └─╴Statement 53:1
CHECK-NEXT:       │              └─╴StmtCompound 53:1
CHECK-NEXT:       │                 ├─╴StmtAssignment 54:3 lhs boolean rhs boolean
CHECK-NEXT:       │                 │  ├─╴AssigVariable 54:3 "b" boolean
CHECK-NEXT:       │                 │  └─╴ExprFunctionCall 54:8 odd boolean
CHECK-NEXT:       │                 │     └─╴ExprVariable 54:12 integer
CHECK-NEXT:       │                 │        └─╴AssigVariable 54:12 "n" integer
CHECK-NEXT:       │                 └─╴StmtEmpty 55:1
CHECK-NEXT:       └─╴Statement 57:1
CHECK-NEXT:          └─╴StmtCompound 57:1
CHECK-NEXT:             └─╴StmtEmpty 58:1

}

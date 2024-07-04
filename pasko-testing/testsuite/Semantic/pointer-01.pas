{
RUN: %pasko --mode=ast-dump --ast-dump-no-ids %s | FileCheck %s
}

program test;
type
  pinteger = ^my_integer;
  my_integer = integer;

var
  a : pinteger;
  b : ^integer;
  c : ^my_integer;

{ main }
begin
  a := b;
  a := c;

  b := a;
  b := c;

  c := a;
  c := b;
end.

{

CHECK: Program 5:1
CHECK-NEXT: ├─╴ProgramHeading 5:1 "test" []
CHECK-NEXT: └─╴ProgramBlock 6:1
CHECK-NEXT:    └─╴Block 6:1
CHECK-NEXT:       ├─╴TypeDefinitionPart 6:1
CHECK-NEXT:       │  ├─╴TypeDefinition 7:3 pinteger
CHECK-NEXT:       │  │  └─╴PointerType 7:14
CHECK-NEXT:       │  │     └─╴TypeIdentifier 7:15 "my_integer" my_integer (an alias of integer)
CHECK-NEXT:       │  └─╴TypeDefinition 8:3 my_integer
CHECK-NEXT:       │     └─╴TypeIdentifier 8:16 "integer" integer
CHECK-NEXT:       ├─╴VariableDeclarationPart 10:1
CHECK-NEXT:       │  ├─╴VariableDeclaration 11:3 ["a"]
CHECK-NEXT:       │  │  └─╴TypeIdentifier 11:7 "pinteger" pinteger (an alias of ^integer)
CHECK-NEXT:       │  ├─╴VariableDeclaration 12:3 ["b"]
CHECK-NEXT:       │  │  └─╴PointerType 12:7
CHECK-NEXT:       │  │     └─╴TypeIdentifier 12:8 "integer" integer
CHECK-NEXT:       │  └─╴VariableDeclaration 13:3 ["c"]
CHECK-NEXT:       │     └─╴PointerType 13:7
CHECK-NEXT:       │        └─╴TypeIdentifier 13:8 "my_integer" my_integer (an alias of integer)
CHECK-NEXT:       └─╴Statement 16:1
CHECK-NEXT:          └─╴StmtCompound 16:1
CHECK-NEXT:             ├─╴StmtAssignment 17:3 lhs pinteger (an alias of ^integer) rhs ^integer
CHECK-NEXT:             │  ├─╴AssigVariable 17:3 "a" pinteger (an alias of ^integer)
CHECK-NEXT:             │  └─╴ExprVariable 17:8 ^integer
CHECK-NEXT:             │     └─╴AssigVariable 17:8 "b" ^integer
CHECK-NEXT:             ├─╴StmtAssignment 18:3 lhs pinteger (an alias of ^integer) rhs ^my_integer (an alias of integer)
CHECK-NEXT:             │  ├─╴AssigVariable 18:3 "a" pinteger (an alias of ^integer)
CHECK-NEXT:             │  └─╴ExprVariable 18:8 ^my_integer (an alias of integer)
CHECK-NEXT:             │     └─╴AssigVariable 18:8 "c" ^my_integer (an alias of integer)
CHECK-NEXT:             ├─╴StmtAssignment 20:3 lhs ^integer rhs pinteger (an alias of ^integer)
CHECK-NEXT:             │  ├─╴AssigVariable 20:3 "b" ^integer
CHECK-NEXT:             │  └─╴ExprVariable 20:8 pinteger (an alias of ^integer)
CHECK-NEXT:             │     └─╴AssigVariable 20:8 "a" pinteger (an alias of ^integer)
CHECK-NEXT:             ├─╴StmtAssignment 21:3 lhs ^integer rhs ^my_integer (an alias of integer)
CHECK-NEXT:             │  ├─╴AssigVariable 21:3 "b" ^integer
CHECK-NEXT:             │  └─╴ExprVariable 21:8 ^my_integer (an alias of integer)
CHECK-NEXT:             │     └─╴AssigVariable 21:8 "c" ^my_integer (an alias of integer)
CHECK-NEXT:             ├─╴StmtAssignment 23:3 lhs ^my_integer (an alias of integer) rhs pinteger (an alias of ^integer)
CHECK-NEXT:             │  ├─╴AssigVariable 23:3 "c" ^my_integer (an alias of integer)
CHECK-NEXT:             │  └─╴ExprVariable 23:8 pinteger (an alias of ^integer)
CHECK-NEXT:             │     └─╴AssigVariable 23:8 "a" pinteger (an alias of ^integer)
CHECK-NEXT:             ├─╴StmtAssignment 24:3 lhs ^my_integer (an alias of integer) rhs ^integer
CHECK-NEXT:             │  ├─╴AssigVariable 24:3 "c" ^my_integer (an alias of integer)
CHECK-NEXT:             │  └─╴ExprVariable 24:8 ^integer
CHECK-NEXT:             │     └─╴AssigVariable 24:8 "b" ^integer
CHECK-NEXT:             └─╴StmtEmpty 25:1

}

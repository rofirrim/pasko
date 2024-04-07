{
RUN: %pasko --mode=ast-dump --ast-dump-no-ids %s 2>&1 | FileCheck %s
}

program test;
type
  my_enum = (a, b, c);

var
  e : my_enum;

{ main }
begin
  e := a;
  e := e;

  case e of
     a : ;
     b, c : ;
  end;

  for e := a to c do
  begin
    ;
  end
end.

{

CHECK: Program 5:1 
CHECK-NEXT: ├─╴ProgramHeading 5:1 "test" []
CHECK-NEXT: └─╴ProgramBlock 6:1 
CHECK-NEXT:    └─╴Block 6:1 
CHECK-NEXT:       ├─╴TypeDefinitionPart 6:1 
CHECK-NEXT:       │  └─╴TypeDefinition 7:3 my_enum
CHECK-NEXT:       │     └─╴EnumeratedType 7:13 a, b, c
CHECK-NEXT:       ├─╴VariableDeclarationPart 9:1 
CHECK-NEXT:       │  └─╴VariableDeclaration 10:3 ["e"]
CHECK-NEXT:       │     └─╴TypeIdentifier 10:7 "my_enum" my_enum (an alias of (a, b, c))
CHECK-NEXT:       └─╴Statement 13:1 
CHECK-NEXT:          └─╴StmtCompound 13:1 
CHECK-NEXT:             ├─╴StmtAssignment 14:3 lhs my_enum (an alias of (a, b, c)) rhs (a, b, c)
CHECK-NEXT:             │  ├─╴AssigVariable 14:3 "e" my_enum (an alias of (a, b, c))
CHECK-NEXT:             │  └─╴ExprConst 14:8 "(a, b, c)" 0
CHECK-NEXT:             │     └─╴ConstNamed 14:8 "a" (a, b, c) 0
CHECK-NEXT:             ├─╴StmtAssignment 15:3 lhs my_enum (an alias of (a, b, c)) rhs my_enum (an alias of (a, b, c))
CHECK-NEXT:             │  ├─╴AssigVariable 15:3 "e" my_enum (an alias of (a, b, c))
CHECK-NEXT:             │  └─╴ExprVariable 15:8 my_enum (an alias of (a, b, c))
CHECK-NEXT:             │     └─╴AssigVariable 15:8 "e" my_enum (an alias of (a, b, c))
CHECK-NEXT:             ├─╴StmtCase 17:3 
CHECK-NEXT:             │  ├─╴ExprVariable 17:8 my_enum (an alias of (a, b, c))
CHECK-NEXT:             │  │  └─╴AssigVariable 17:8 "e" my_enum (an alias of (a, b, c))
CHECK-NEXT:             │  ├─╴CaseListElement 18:6 
CHECK-NEXT:             │  │  ├─╴ConstNamed 18:6 "a" (a, b, c) 0
CHECK-NEXT:             │  │  └─╴StmtEmpty 18:10 
CHECK-NEXT:             │  └─╴CaseListElement 19:6 
CHECK-NEXT:             │     ├─╴ConstNamed 19:6 "b" (a, b, c) 1
CHECK-NEXT:             │     ├─╴ConstNamed 19:9 "c" (a, b, c) 2
CHECK-NEXT:             │     └─╴StmtEmpty 19:13 
CHECK-NEXT:             └─╴StmtFor 22:3 to
CHECK-NEXT:                ├─╴AssigVariable 22:7 "e" my_enum (an alias of (a, b, c))
CHECK-NEXT:                ├─╴ExprConst 22:12 "(a, b, c)" 0
CHECK-NEXT:                │  └─╴ConstNamed 22:12 "a" (a, b, c) 0
CHECK-NEXT:                ├─╴ExprConst 22:17 "(a, b, c)" 2
CHECK-NEXT:                │  └─╴ConstNamed 22:17 "c" (a, b, c) 2
CHECK-NEXT:                └─╴StmtCompound 23:3 
CHECK-NEXT:                   ├─╴StmtEmpty 24:5 
CHECK-NEXT:                   └─╴StmtEmpty 25:3 

}

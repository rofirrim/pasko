{
RUN: %pasko --mode=ast-dump --ast-dump-no-ids %s 2>&1 | FileCheck %s
}

program test;
type
  my_enum = (v_a, v_b, v_c, v_d, v_e);
  my_subrange = -10..+10;
  my_subrange2 = v_b..v_e;

var
  sub: my_subrange;
  sub2 : my_subrange2;

{ main }
begin
   sub := 34;
   sub := sub;

   sub2 := v_b;
   sub2 := sub2;
   sub2 := v_a; { FIXME: this could be diagnosed }
end.

{

CHECK: Program 5:1 
CHECK-NEXT: ├─╴ProgramHeading 5:1 "test" []
CHECK-NEXT: └─╴ProgramBlock 6:1 
CHECK-NEXT:    └─╴Block 6:1 
CHECK-NEXT:       ├─╴TypeDefinitionPart 6:1 
CHECK-NEXT:       │  ├─╴TypeDefinition 7:3 my_enum
CHECK-NEXT:       │  │  └─╴EnumeratedType 7:13 v_a, v_b, v_c, v_d, v_e
CHECK-NEXT:       │  ├─╴TypeDefinition 8:3 my_subrange
CHECK-NEXT:       │  │  └─╴SubRangeType 8:17 
CHECK-NEXT:       │  │     ├─╴ConstInteger 8:17 -10 integer
CHECK-NEXT:       │  │     └─╴ConstInteger 8:22 10 integer
CHECK-NEXT:       │  └─╴TypeDefinition 9:3 my_subrange2
CHECK-NEXT:       │     └─╴SubRangeType 9:18 
CHECK-NEXT:       │        ├─╴ConstNamed 9:18 "v_b" (v_a, v_b, v_c, v_d, v_e) 1
CHECK-NEXT:       │        └─╴ConstNamed 9:23 "v_e" (v_a, v_b, v_c, v_d, v_e) 4
CHECK-NEXT:       ├─╴VariableDeclarationPart 11:1 
CHECK-NEXT:       │  ├─╴VariableDeclaration 12:3 ["sub"]
CHECK-NEXT:       │  │  └─╴TypeIdentifier 12:8 "my_subrange" my_subrange (an alias of -10..10)
CHECK-NEXT:       │  └─╴VariableDeclaration 13:3 ["sub2"]
CHECK-NEXT:       │     └─╴TypeIdentifier 13:10 "my_subrange2" my_subrange2 (an alias of 1..4)
CHECK-NEXT:       └─╴Statement 16:1 
CHECK-NEXT:          └─╴StmtCompound 16:1 
CHECK-NEXT:             ├─╴StmtAssignment 17:4 lhs my_subrange (an alias of -10..10) rhs integer
CHECK-NEXT:             │  ├─╴AssigVariable 17:4 "sub" my_subrange (an alias of -10..10)
CHECK-NEXT:             │  └─╴ExprConst 17:11 "integer" <<no const>>
CHECK-NEXT:             │     └─╴ConstInteger 17:11 34 integer
CHECK-NEXT:             ├─╴StmtAssignment 18:4 lhs my_subrange (an alias of -10..10) rhs my_subrange (an alias of -10..10)
CHECK-NEXT:             │  ├─╴AssigVariable 18:4 "sub" my_subrange (an alias of -10..10)
CHECK-NEXT:             │  └─╴ExprVariable 18:11 my_subrange (an alias of -10..10)
CHECK-NEXT:             │     └─╴AssigVariable 18:11 "sub" my_subrange (an alias of -10..10)
CHECK-NEXT:             ├─╴StmtAssignment 20:4 lhs my_subrange2 (an alias of 1..4) rhs (v_a, v_b, v_c, v_d, v_e)
CHECK-NEXT:             │  ├─╴AssigVariable 20:4 "sub2" my_subrange2 (an alias of 1..4)
CHECK-NEXT:             │  └─╴ExprConst 20:12 "(v_a, v_b, v_c, v_d, v_e)" 1
CHECK-NEXT:             │     └─╴ConstNamed 20:12 "v_b" (v_a, v_b, v_c, v_d, v_e) 1
CHECK-NEXT:             ├─╴StmtAssignment 21:4 lhs my_subrange2 (an alias of 1..4) rhs my_subrange2 (an alias of 1..4)
CHECK-NEXT:             │  ├─╴AssigVariable 21:4 "sub2" my_subrange2 (an alias of 1..4)
CHECK-NEXT:             │  └─╴ExprVariable 21:12 my_subrange2 (an alias of 1..4)
CHECK-NEXT:             │     └─╴AssigVariable 21:12 "sub2" my_subrange2 (an alias of 1..4)
CHECK-NEXT:             ├─╴StmtAssignment 22:4 lhs my_subrange2 (an alias of 1..4) rhs (v_a, v_b, v_c, v_d, v_e)
CHECK-NEXT:             │  ├─╴AssigVariable 22:4 "sub2" my_subrange2 (an alias of 1..4)
CHECK-NEXT:             │  └─╴ExprConst 22:12 "(v_a, v_b, v_c, v_d, v_e)" 0
CHECK-NEXT:             │     └─╴ConstNamed 22:12 "v_a" (v_a, v_b, v_c, v_d, v_e) 0
CHECK-NEXT:             └─╴StmtEmpty 23:1 

}

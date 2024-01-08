{
RUN: %pasko --mode=ast-dump-pre --ast-dump-no-ids %s | FileCheck %s
}

program main;
begin
  begin
    a := 3
  end;

  if b then
    a := 3
  else
    a := 4;
  
  while b do 
    a := 3;

  repeat
    a := 3;
    a := 4;
  until b;

  for i := 1 to 10 do
    a := 3;

  for i := 10 downto 1 do
    a := 3;
  
end.

{

CHECK:      Program 5:1 
CHECK-NEXT: ├─╴ProgramHeading 5:1 "main" []
CHECK-NEXT: └─╴ProgramBlock 6:1 
CHECK-NEXT:    └─╴Block 6:1 
CHECK-NEXT:       └─╴Statement 6:1 
CHECK-NEXT:          └─╴StmtCompound 6:1 
CHECK-NEXT:             ├─╴StmtCompound 7:3 
CHECK-NEXT:             │  └─╴StmtAssignment 8:5 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:             │     ├─╴AssigVariable 8:5 "a" <<no-type>>
CHECK-NEXT:             │     └─╴ExprConst 8:10 "<<no-type>>" <<no const>>
CHECK-NEXT:             │        └─╴ConstInteger 8:10 3 <<no-type>>
CHECK-NEXT:             ├─╴StmtIf 11:3 
CHECK-NEXT:             │  ├─╴ExprVariable 11:6 <<no-type>>
CHECK-NEXT:             │  │  └─╴AssigVariable 11:6 "b" <<no-type>>
CHECK-NEXT:             │  ├─╴StmtAssignment 12:5 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:             │  │  ├─╴AssigVariable 12:5 "a" <<no-type>>
CHECK-NEXT:             │  │  └─╴ExprConst 12:10 "<<no-type>>" <<no const>>
CHECK-NEXT:             │  │     └─╴ConstInteger 12:10 3 <<no-type>>
CHECK-NEXT:             │  └─╴StmtAssignment 14:5 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:             │     ├─╴AssigVariable 14:5 "a" <<no-type>>
CHECK-NEXT:             │     └─╴ExprConst 14:10 "<<no-type>>" <<no const>>
CHECK-NEXT:             │        └─╴ConstInteger 14:10 4 <<no-type>>
CHECK-NEXT:             ├─╴StmtWhileDo 16:3 
CHECK-NEXT:             │  ├─╴ExprVariable 16:9 <<no-type>>
CHECK-NEXT:             │  │  └─╴AssigVariable 16:9 "b" <<no-type>>
CHECK-NEXT:             │  └─╴StmtAssignment 17:5 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:             │     ├─╴AssigVariable 17:5 "a" <<no-type>>
CHECK-NEXT:             │     └─╴ExprConst 17:10 "<<no-type>>" <<no const>>
CHECK-NEXT:             │        └─╴ConstInteger 17:10 3 <<no-type>>
CHECK-NEXT:             ├─╴StmtRepeatUntil 19:3 
CHECK-NEXT:             │  ├─╴StmtAssignment 20:5 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:             │  │  ├─╴AssigVariable 20:5 "a" <<no-type>>
CHECK-NEXT:             │  │  └─╴ExprConst 20:10 "<<no-type>>" <<no const>>
CHECK-NEXT:             │  │     └─╴ConstInteger 20:10 3 <<no-type>>
CHECK-NEXT:             │  ├─╴StmtAssignment 21:5 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:             │  │  ├─╴AssigVariable 21:5 "a" <<no-type>>
CHECK-NEXT:             │  │  └─╴ExprConst 21:10 "<<no-type>>" <<no const>>
CHECK-NEXT:             │  │     └─╴ConstInteger 21:10 4 <<no-type>>
CHECK-NEXT:             │  └─╴ExprVariable 22:9 <<no-type>>
CHECK-NEXT:             │     └─╴AssigVariable 22:9 "b" <<no-type>>
CHECK-NEXT:             ├─╴StmtFor 24:3 to
CHECK-NEXT:             │  ├─╴AssigVariable 24:7 "i" <<no-type>>
CHECK-NEXT:             │  ├─╴ExprConst 24:12 "<<no-type>>" <<no const>>
CHECK-NEXT:             │  │  └─╴ConstInteger 24:12 1 <<no-type>>
CHECK-NEXT:             │  ├─╴ExprConst 24:17 "<<no-type>>" <<no const>>
CHECK-NEXT:             │  │  └─╴ConstInteger 24:17 10 <<no-type>>
CHECK-NEXT:             │  └─╴StmtAssignment 25:5 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:             │     ├─╴AssigVariable 25:5 "a" <<no-type>>
CHECK-NEXT:             │     └─╴ExprConst 25:10 "<<no-type>>" <<no const>>
CHECK-NEXT:             │        └─╴ConstInteger 25:10 3 <<no-type>>
CHECK-NEXT:             └─╴StmtFor 27:3 downto
CHECK-NEXT:                ├─╴AssigVariable 27:7 "i" <<no-type>>
CHECK-NEXT:                ├─╴ExprConst 27:12 "<<no-type>>" <<no const>>
CHECK-NEXT:                │  └─╴ConstInteger 27:12 10 <<no-type>>
CHECK-NEXT:                ├─╴ExprConst 27:22 "<<no-type>>" <<no const>>
CHECK-NEXT:                │  └─╴ConstInteger 27:22 1 <<no-type>>
CHECK-NEXT:                └─╴StmtAssignment 28:5 lhs <<no-type>> rhs <<no-type>>
CHECK-NEXT:                   ├─╴AssigVariable 28:5 "a" <<no-type>>
CHECK-NEXT:                   └─╴ExprConst 28:10 "<<no-type>>" <<no const>>
CHECK-NEXT:                      └─╴ConstInteger 28:10 3 <<no-type>>

}

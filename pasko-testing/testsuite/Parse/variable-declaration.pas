{
RUN: %pasko --mode=ast-dump-pre --ast-dump-no-ids %s | FileCheck %s
}

program main;
var
  x, y, z, max : real;
  i, j : integer;
  k : 0..9;
  p, q, r : Boolean;
  operator : (plus, minus, times);
  a : array [0..63] of real;
  c : colour;
  f : file of char;
  hue1, hue2 : set of colour;
  p1, p2 : person;
  m, m1, m2 : array [1..10, 1..20] of real;
  coord : polar;
  pooltape : array [1..4] of FileOfInteger;
  date : record
     month : 1..12;
   year : integer
  end;
begin
end.

{

CHECK:      Program 5:1 
CHECK-NEXT: ├─╴ProgramHeading 5:1 "main" []
CHECK-NEXT: └─╴ProgramBlock 6:1 
CHECK-NEXT:    └─╴Block 6:1 
CHECK-NEXT:       ├─╴VariableDeclarationPart 6:1 
CHECK-NEXT:       │  ├─╴VariableDeclaration 7:3 ["x", "y", "z", "max"]
CHECK-NEXT:       │  │  └─╴TypeIdentifier 7:18 "real" <<no-type>>
CHECK-NEXT:       │  ├─╴VariableDeclaration 8:3 ["i", "j"]
CHECK-NEXT:       │  │  └─╴TypeIdentifier 8:10 "integer" <<no-type>>
CHECK-NEXT:       │  ├─╴VariableDeclaration 9:3 ["k"]
CHECK-NEXT:       │  │  └─╴SubRangeType 9:7 
CHECK-NEXT:       │  │     ├─╴ConstInteger 9:7 0 <<no-type>>
CHECK-NEXT:       │  │     └─╴ConstInteger 9:10 9 <<no-type>>
CHECK-NEXT:       │  ├─╴VariableDeclaration 10:3 ["p", "q", "r"]
CHECK-NEXT:       │  │  └─╴TypeIdentifier 10:13 "boolean" <<no-type>>
CHECK-NEXT:       │  ├─╴VariableDeclaration 11:3 ["operator"]
CHECK-NEXT:       │  │  └─╴EnumeratedType 11:14 plus, minus, times
CHECK-NEXT:       │  ├─╴VariableDeclaration 12:3 ["a"]
CHECK-NEXT:       │  │  └─╴ArrayType 12:7 (unpacked)
CHECK-NEXT:       │  │     ├─╴SubRangeType 12:14 
CHECK-NEXT:       │  │     │  ├─╴ConstInteger 12:14 0 <<no-type>>
CHECK-NEXT:       │  │     │  └─╴ConstInteger 12:17 63 <<no-type>>
CHECK-NEXT:       │  │     └─╴TypeIdentifier 12:24 "real" <<no-type>>
CHECK-NEXT:       │  ├─╴VariableDeclaration 13:3 ["c"]
CHECK-NEXT:       │  │  └─╴TypeIdentifier 13:7 "colour" <<no-type>>
CHECK-NEXT:       │  ├─╴VariableDeclaration 14:3 ["f"]
CHECK-NEXT:       │  │  └─╴FileType 14:7 (packed)
CHECK-NEXT:       │  │     └─╴TypeIdentifier 14:15 "char" <<no-type>>
CHECK-NEXT:       │  ├─╴VariableDeclaration 15:3 ["hue1", "hue2"]
CHECK-NEXT:       │  │  └─╴SetType 15:16 (packed)
CHECK-NEXT:       │  │     └─╴TypeIdentifier 15:23 "colour" <<no-type>>
CHECK-NEXT:       │  ├─╴VariableDeclaration 16:3 ["p1", "p2"]
CHECK-NEXT:       │  │  └─╴TypeIdentifier 16:12 "person" <<no-type>>
CHECK-NEXT:       │  ├─╴VariableDeclaration 17:3 ["m", "m1", "m2"]
CHECK-NEXT:       │  │  └─╴ArrayType 17:15 (unpacked)
CHECK-NEXT:       │  │     ├─╴SubRangeType 17:22 
CHECK-NEXT:       │  │     │  ├─╴ConstInteger 17:22 1 <<no-type>>
CHECK-NEXT:       │  │     │  └─╴ConstInteger 17:25 10 <<no-type>>
CHECK-NEXT:       │  │     ├─╴SubRangeType 17:29 
CHECK-NEXT:       │  │     │  ├─╴ConstInteger 17:29 1 <<no-type>>
CHECK-NEXT:       │  │     │  └─╴ConstInteger 17:32 20 <<no-type>>
CHECK-NEXT:       │  │     └─╴TypeIdentifier 17:39 "real" <<no-type>>
CHECK-NEXT:       │  ├─╴VariableDeclaration 18:3 ["coord"]
CHECK-NEXT:       │  │  └─╴TypeIdentifier 18:11 "polar" <<no-type>>
CHECK-NEXT:       │  ├─╴VariableDeclaration 19:3 ["pooltape"]
CHECK-NEXT:       │  │  └─╴ArrayType 19:14 (unpacked)
CHECK-NEXT:       │  │     ├─╴SubRangeType 19:21 
CHECK-NEXT:       │  │     │  ├─╴ConstInteger 19:21 1 <<no-type>>
CHECK-NEXT:       │  │     │  └─╴ConstInteger 19:24 4 <<no-type>>
CHECK-NEXT:       │  │     └─╴TypeIdentifier 19:30 "fileofinteger" <<no-type>>
CHECK-NEXT:       │  └─╴VariableDeclaration 20:3 ["date"]
CHECK-NEXT:       │     └─╴RecordType 20:10 (packed)
CHECK-NEXT:       │        ├─╴RecordSection 21:6 month
CHECK-NEXT:       │        │  └─╴SubRangeType 21:14 
CHECK-NEXT:       │        │     ├─╴ConstInteger 21:14 1 <<no-type>>
CHECK-NEXT:       │        │     └─╴ConstInteger 21:17 12 <<no-type>>
CHECK-NEXT:       │        └─╴RecordSection 22:4 year
CHECK-NEXT:       │           └─╴TypeIdentifier 22:11 "integer" <<no-type>>
CHECK-NEXT:       └─╴Statement 24:1 
CHECK-NEXT:          └─╴StmtCompound 24:1 

}

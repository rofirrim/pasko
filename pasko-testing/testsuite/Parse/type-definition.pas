{
RUN: %pasko --mode=ast-dump-pre --ast-dump-no-ids %s | FileCheck %s
}

program main;
type
   natural = 0..maxint;
   count = integer;
   range = integer;
   colour = (red, yellow, green, blue);
   sex = (male, female);
   year = 1900..1999;
   shape = (triangle, rectangle, circle);
   punchedcard = array [1..80] of char;
   charsequence = file of char;
   polar = record
     r : real;
     theta : angle
   end;
   indextype = 1..limit;
   vector = array [indextype] of real;
   person = ^persondetails;
   persondetails = record
     name, firstname : charsequence;
     age : natural;
     married : Boolean;
     father, child, sibling : person ;
      case s : sex of
       male :
         (enlisted, bearded : Boolean);
      female :
         (mother, programmer : Boolean)
   end;
   FileOfInteger = file of integer;

begin
end.

{

CHECK: Program 5:1
CHECK-NEXT: ├─╴ProgramHeading 5:1 "main" []
CHECK-NEXT: └─╴ProgramBlock 6:1
CHECK-NEXT:    └─╴Block 6:1
CHECK-NEXT:       ├─╴TypeDefinitionPart 6:1
CHECK-NEXT:       │  ├─╴TypeDefinition 7:4 natural
CHECK-NEXT:       │  │  └─╴SubRangeType 7:14
CHECK-NEXT:       │  │     ├─╴ConstInteger 7:14 0 <<no-type>>
CHECK-NEXT:       │  │     └─╴ConstNamed 7:17 "maxint" <<no-type>> <<no const>>
CHECK-NEXT:       │  ├─╴TypeDefinition 8:4 count
CHECK-NEXT:       │  │  └─╴TypeIdentifier 8:12 "integer" <<no-type>>
CHECK-NEXT:       │  ├─╴TypeDefinition 9:4 range
CHECK-NEXT:       │  │  └─╴TypeIdentifier 9:12 "integer" <<no-type>>
CHECK-NEXT:       │  ├─╴TypeDefinition 10:4 colour
CHECK-NEXT:       │  │  └─╴EnumeratedType 10:13 red, yellow, green, blue
CHECK-NEXT:       │  ├─╴TypeDefinition 11:4 sex
CHECK-NEXT:       │  │  └─╴EnumeratedType 11:10 male, female
CHECK-NEXT:       │  ├─╴TypeDefinition 12:4 year
CHECK-NEXT:       │  │  └─╴SubRangeType 12:11
CHECK-NEXT:       │  │     ├─╴ConstInteger 12:11 1900 <<no-type>>
CHECK-NEXT:       │  │     └─╴ConstInteger 12:17 1999 <<no-type>>
CHECK-NEXT:       │  ├─╴TypeDefinition 13:4 shape
CHECK-NEXT:       │  │  └─╴EnumeratedType 13:12 triangle, rectangle, circle
CHECK-NEXT:       │  ├─╴TypeDefinition 14:4 punchedcard
CHECK-NEXT:       │  │  └─╴ArrayType 14:18 (unpacked)
CHECK-NEXT:       │  │     ├─╴SubRangeType 14:25
CHECK-NEXT:       │  │     │  ├─╴ConstInteger 14:25 1 <<no-type>>
CHECK-NEXT:       │  │     │  └─╴ConstInteger 14:28 80 <<no-type>>
CHECK-NEXT:       │  │     └─╴TypeIdentifier 14:35 "char" <<no-type>>
CHECK-NEXT:       │  ├─╴TypeDefinition 15:4 charsequence
CHECK-NEXT:       │  │  └─╴FileType 15:19 (unpacked)
CHECK-NEXT:       │  │     └─╴TypeIdentifier 15:27 "char" <<no-type>>
CHECK-NEXT:       │  ├─╴TypeDefinition 16:4 polar
CHECK-NEXT:       │  │  └─╴RecordType 16:12 (unpacked)
CHECK-NEXT:       │  │     └─╴FieldList 17:6
CHECK-NEXT:       │  │        ├─╴RecordSection 17:6 r
CHECK-NEXT:       │  │        │  └─╴TypeIdentifier 17:10 "real" <<no-type>>
CHECK-NEXT:       │  │        └─╴RecordSection 18:6 theta
CHECK-NEXT:       │  │           └─╴TypeIdentifier 18:14 "angle" <<no-type>>
CHECK-NEXT:       │  ├─╴TypeDefinition 20:4 indextype
CHECK-NEXT:       │  │  └─╴SubRangeType 20:16
CHECK-NEXT:       │  │     ├─╴ConstInteger 20:16 1 <<no-type>>
CHECK-NEXT:       │  │     └─╴ConstNamed 20:19 "limit" <<no-type>> <<no const>>
CHECK-NEXT:       │  ├─╴TypeDefinition 21:4 vector
CHECK-NEXT:       │  │  └─╴ArrayType 21:13 (unpacked)
CHECK-NEXT:       │  │     ├─╴TypeIdentifier 21:20 "indextype" <<no-type>>
CHECK-NEXT:       │  │     └─╴TypeIdentifier 21:34 "real" <<no-type>>
CHECK-NEXT:       │  ├─╴TypeDefinition 22:4 person
CHECK-NEXT:       │  │  └─╴PointerType 22:13
CHECK-NEXT:       │  │     └─╴TypeIdentifier 22:14 "persondetails" <<no-type>>
CHECK-NEXT:       │  ├─╴TypeDefinition 23:4 persondetails
CHECK-NEXT:       │  │  └─╴RecordType 23:20 (unpacked)
CHECK-NEXT:       │  │     └─╴FieldList 24:6
CHECK-NEXT:       │  │        ├─╴RecordSection 24:6 name, firstname
CHECK-NEXT:       │  │        │  └─╴TypeIdentifier 24:24 "charsequence" <<no-type>>
CHECK-NEXT:       │  │        ├─╴RecordSection 25:6 age
CHECK-NEXT:       │  │        │  └─╴TypeIdentifier 25:12 "natural" <<no-type>>
CHECK-NEXT:       │  │        ├─╴RecordSection 26:6 married
CHECK-NEXT:       │  │        │  └─╴TypeIdentifier 26:16 "boolean" <<no-type>>
CHECK-NEXT:       │  │        ├─╴RecordSection 27:6 father, child, sibling
CHECK-NEXT:       │  │        │  └─╴TypeIdentifier 27:31 "person" <<no-type>>
CHECK-NEXT:       │  │        └─╴VariantPart 28:7
CHECK-NEXT:       │  │           ├─╴VariantSelector 28:12 s
CHECK-NEXT:       │  │           │  └─╴TypeIdentifier 28:16 "sex" <<no-type>>
CHECK-NEXT:       │  │           ├─╴Variant 29:8
CHECK-NEXT:       │  │           │  ├─╴ConstNamed 29:8 "male" <<no-type>> <<no const>>
CHECK-NEXT:       │  │           │  └─╴FieldList 30:11
CHECK-NEXT:       │  │           │     └─╴RecordSection 30:11 enlisted, bearded
CHECK-NEXT:       │  │           │        └─╴TypeIdentifier 30:31 "boolean" <<no-type>>
CHECK-NEXT:       │  │           └─╴Variant 31:7
CHECK-NEXT:       │  │              ├─╴ConstNamed 31:7 "female" <<no-type>> <<no const>>
CHECK-NEXT:       │  │              └─╴FieldList 32:11
CHECK-NEXT:       │  │                 └─╴RecordSection 32:11 mother, programmer
CHECK-NEXT:       │  │                    └─╴TypeIdentifier 32:32 "boolean" <<no-type>>
CHECK-NEXT:       │  └─╴TypeDefinition 34:4 fileofinteger
CHECK-NEXT:       │     └─╴FileType 34:20 (unpacked)
CHECK-NEXT:       │        └─╴TypeIdentifier 34:28 "integer" <<no-type>>
CHECK-NEXT:       └─╴Statement 36:1
CHECK-NEXT:          └─╴StmtCompound 36:1
CHECK-NEXT:             └─╴StmtEmpty 37:1 

}

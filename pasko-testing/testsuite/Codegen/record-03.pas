{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main;

type
  my_record_1 = record
     b: boolean;
     i: integer;
     x: real;
     c: char;
  end;
  my_record_2 = record
     i: integer;
     x: real;
     c: char;
     b: boolean;
  end;

procedure foo_1;
var
  r: my_record_1;
begin
end;

procedure foo_2;
var
  r: my_record_2;
begin
end;

begin
end.

{

CHECK:      *** IR for procedure 'foo_1'
CHECK-NEXT: function u0:0() system_v {
CHECK-NEXT:     ss0 = explicit_slot 32
CHECK-EMPTY:
CHECK-NEXT: block0:
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for procedure 'foo_1' seems OK

CHECK:      *** IR for procedure 'foo_2'
CHECK-NEXT: function u0:1() system_v {
CHECK-NEXT:     ss0 = explicit_slot 24
CHECK-EMPTY:
CHECK-NEXT: block0:
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for procedure 'foo_2' seems OK

}

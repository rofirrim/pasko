{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main;

type
  my_record = record
    x: integer;
    y: real;
  end;

  my_array = array [1..10] of my_record;

procedure foo;
var
  a: my_array;
begin
  a[1].x := 42;
end;

begin
end.

{

CHECK: *** IR for 'foo'
CHECK-NEXT: function u0:0() system_v {
CHECK-NEXT:     ss0 = explicit_slot 160 ; a
CHECK-EMPTY:
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = iconst.i64 42
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     v2 = iconst.i64 1
CHECK-NEXT:     v3 = iconst.i64 16
CHECK-NEXT:     v4 = iconst.i64 1
CHECK-NEXT:     v5 = isub v4, v2  ; v4 = 1, v2 = 1
CHECK-NEXT:     v6 = imul v5, v3  ; v3 = 16
CHECK-NEXT:     v7 = iadd v1, v6
CHECK-NEXT:     store v0, v7  ; v0 = 42
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'foo' seems OK

}

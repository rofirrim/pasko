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

CHECK: *** IR for procedure 'foo'
CHECK-NEXT: function u0:9() system_v {
CHECK-NEXT:     ss0 = explicit_slot 160
CHECK-EMPTY:
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = iconst.i64 42
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     v2 = iconst.i64 1
CHECK-NEXT:     v3 = iconst.i64 1
CHECK-NEXT:     v4 = isub v2, v3  ; v2 = 1, v3 = 1
CHECK-NEXT:     v5 = iconst.i64 16
CHECK-NEXT:     v6 = imul v4, v5  ; v5 = 16
CHECK-NEXT:     v7 = iadd v1, v6
CHECK-NEXT:     v8 = iconst.i64 0
CHECK-NEXT:     v9 = iadd v7, v8  ; v8 = 0
CHECK-NEXT:     store v0, v9  ; v0 = 42
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for procedure 'foo' seems OK

}

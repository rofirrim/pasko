{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main;

type
  my_array = array [1..10] of integer;

  my_record = record
    b: boolean;
    a: my_array;
  end;

procedure foo_2;
var
  r: my_record;
begin
  r.b := true;
  r.a[1] := 3;
end;

begin
end.

{

CHECK: *** IR for 'foo_2'
CHECK-NEXT: function u0:0() system_v {
CHECK-NEXT:     ss0 = explicit_slot 88 ; r
CHECK-EMPTY:
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = iconst.i8 1
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v1  ; v0 = 1
CHECK-NEXT:     v2 = iconst.i64 3
CHECK-NEXT:     v3 = stack_addr.i64 ss0
CHECK-NEXT:     v4 = iconst.i64 8
CHECK-NEXT:     v5 = iadd v3, v4  ; v4 = 8
CHECK-NEXT:     v6 = iconst.i64 1
CHECK-NEXT:     v7 = iconst.i64 8
CHECK-NEXT:     v8 = iconst.i64 1
CHECK-NEXT:     v9 = isub v8, v6  ; v8 = 1, v6 = 1
CHECK-NEXT:     v10 = imul v9, v7  ; v7 = 8
CHECK-NEXT:     v11 = iadd v5, v10
CHECK-NEXT:     store v2, v11  ; v2 = 3
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'foo_2' seems OK

}

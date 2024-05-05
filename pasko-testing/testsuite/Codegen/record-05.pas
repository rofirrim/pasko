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

CHECK: *** IR for procedure 'foo_2'
CHECK-NEXT: function u0:19() system_v {
CHECK-NEXT:     ss0 = explicit_slot 88
CHECK-EMPTY:
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = iconst.i8 1
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     v2 = iconst.i64 0
CHECK-NEXT:     v3 = iadd v1, v2  ; v2 = 0
CHECK-NEXT:     store v0, v3  ; v0 = 1
CHECK-NEXT:     v4 = iconst.i64 3
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     v6 = iconst.i64 8
CHECK-NEXT:     v7 = iadd v5, v6  ; v6 = 8
CHECK-NEXT:     v8 = iconst.i64 1
CHECK-NEXT:     v9 = iconst.i64 1
CHECK-NEXT:     v10 = isub v8, v9  ; v8 = 1, v9 = 1
CHECK-NEXT:     v11 = iconst.i64 8
CHECK-NEXT:     v12 = imul v10, v11  ; v11 = 8
CHECK-NEXT:     v13 = iadd v7, v12
CHECK-NEXT:     store v4, v13  ; v4 = 3
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for procedure 'foo_2' seems OK

}

{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main;

type
  my_matrix = array[1..9] of array [10..19] of integer;

var
  a: my_matrix;

procedure foo(var x: my_matrix);
begin
  x[1][10] := 42;
end;

begin
  foo(a);
end.

{

CHECK: *** IR for procedure 'foo'
CHECK-NEXT: function u0:9(i64) system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = iconst.i64 42
CHECK-NEXT:     v3 = stack_addr.i64 ss0
CHECK-NEXT:     v4 = load.i64 v3
CHECK-NEXT:     v5 = iconst.i64 1
CHECK-NEXT:     v6 = iconst.i64 1
CHECK-NEXT:     v7 = isub v5, v6  ; v5 = 1, v6 = 1
CHECK-NEXT:     v8 = iconst.i64 80
CHECK-NEXT:     v9 = imul v7, v8  ; v8 = 80
CHECK-NEXT:     v10 = iadd v4, v9
CHECK-NEXT:     v11 = iconst.i64 10
CHECK-NEXT:     v12 = iconst.i64 10
CHECK-NEXT:     v13 = isub v11, v12  ; v11 = 10, v12 = 10
CHECK-NEXT:     v14 = iconst.i64 8
CHECK-NEXT:     v15 = imul v13, v14  ; v14 = 8
CHECK-NEXT:     v16 = iadd v10, v15
CHECK-NEXT:     store v2, v16  ; v2 = 42
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for procedure 'foo' seems OK

CHECK: *** IR for main
CHECK-NEXT: function u0:10(i32, i64) -> i32 system_v {
CHECK-NEXT:     gv0 = symbol colocated userextname0
CHECK-NEXT:     sig0 = (i64) system_v
CHECK-NEXT:     fn0 = colocated u0:9 sig0
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i32, v1: i64):
CHECK-NEXT:     v2 = global_value.i64 gv0
CHECK-NEXT:     call fn0(v2)
CHECK-NEXT:     v3 = iconst.i32 0
CHECK-NEXT:     return v3  ; v3 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for main seems OK


}

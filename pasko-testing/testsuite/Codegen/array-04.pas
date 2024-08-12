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
CHECK-NEXT: function u0:0(i64) system_v {
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
CHECK-NEXT: function u0:1(i32, i64) -> i32 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-NEXT:     gv0 = symbol colocated userextname4
CHECK-NEXT:     gv1 = symbol colocated userextname5
CHECK-NEXT:     gv2 = symbol colocated userextname6
CHECK-NEXT:     sig0 = (i32, i64, i32, i64, i32, i64) system_v
CHECK-NEXT:     sig1 = (i32, i64) system_v
CHECK-NEXT:     sig2 = () -> i64 system_v
CHECK-NEXT:     sig3 = () -> i64 system_v
CHECK-NEXT:     sig4 = (i64) system_v
CHECK-NEXT:     fn0 = u0:2 sig0
CHECK-NEXT:     fn1 = u0:3 sig1
CHECK-NEXT:     fn2 = u0:4 sig2
CHECK-NEXT:     fn3 = u0:5 sig3
CHECK-NEXT:     fn4 = colocated u0:0 sig4
CHECK-EMPTY: 
CHECK-NEXT: block0(v0: i32, v1: i64):
CHECK-NEXT:     v2 = iconst.i32 0
CHECK-NEXT:     v3 = iconst.i64 0
CHECK-NEXT:     stack_store v3, ss0  ; v3 = 0
CHECK-NEXT:     v4 = stack_addr.i64 ss0
CHECK-NEXT:     v5 = iconst.i64 0
CHECK-NEXT:     stack_store v5, ss1  ; v5 = 0
CHECK-NEXT:     v6 = stack_addr.i64 ss1
CHECK-NEXT:     v7 = iconst.i32 0
CHECK-NEXT:     call fn0(v0, v1, v2, v4, v7, v6)  ; v2 = 0, v7 = 0
CHECK-NEXT:     v8 = call fn2()
CHECK-NEXT:     v9 = global_value.i64 gv0
CHECK-NEXT:     store v8, v9
CHECK-NEXT:     v10 = call fn3()
CHECK-NEXT:     v11 = global_value.i64 gv1
CHECK-NEXT:     store v10, v11
CHECK-NEXT:     v12 = global_value.i64 gv2
CHECK-NEXT:     call fn4(v12)
CHECK-NEXT:     call fn1(v7, v6)  ; v7 = 0
CHECK-NEXT:     v13 = iconst.i32 0
CHECK-NEXT:     return v13  ; v13 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for main seems OK

}

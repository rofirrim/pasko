{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main;

var
  a: array[1..9] of array [10..19] of integer;

begin
  a[1][10] := 3;
end.

{

CHECK: *** IR for main
CHECK-NEXT: function u0:38(i32, i64) -> i32 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-NEXT:     gv0 = symbol colocated userextname4
CHECK-NEXT:     gv1 = symbol colocated userextname5
CHECK-NEXT:     gv2 = symbol colocated userextname6
CHECK-NEXT:     sig0 = (i32, i64, i32, i64, i32, i64) system_v
CHECK-NEXT:     sig1 = (i32, i64) system_v
CHECK-NEXT:     sig2 = () -> i64 system_v
CHECK-NEXT:     sig3 = () -> i64 system_v
CHECK-NEXT:     fn0 = u0:21 sig0
CHECK-NEXT:     fn1 = u0:22 sig1
CHECK-NEXT:     fn2 = u0:23 sig2
CHECK-NEXT:     fn3 = u0:24 sig3
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
CHECK-NEXT:     v12 = iconst.i64 3
CHECK-NEXT:     v13 = global_value.i64 gv2
CHECK-NEXT:     v14 = iconst.i64 1
CHECK-NEXT:     v15 = iconst.i64 1
CHECK-NEXT:     v16 = isub v14, v15  ; v14 = 1, v15 = 1
CHECK-NEXT:     v17 = iconst.i64 80
CHECK-NEXT:     v18 = imul v16, v17  ; v17 = 80
CHECK-NEXT:     v19 = iadd v13, v18
CHECK-NEXT:     v20 = iconst.i64 10
CHECK-NEXT:     v21 = iconst.i64 10
CHECK-NEXT:     v22 = isub v20, v21  ; v20 = 10, v21 = 10
CHECK-NEXT:     v23 = iconst.i64 8
CHECK-NEXT:     v24 = imul v22, v23  ; v23 = 8
CHECK-NEXT:     v25 = iadd v19, v24
CHECK-NEXT:     store v12, v25  ; v12 = 3
CHECK-NEXT:     call fn1(v7, v6)  ; v7 = 0
CHECK-NEXT:     v26 = iconst.i32 0
CHECK-NEXT:     return v26  ; v26 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for main seems OK

}

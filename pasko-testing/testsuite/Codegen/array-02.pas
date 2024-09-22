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

CHECK: *** IR for 'main'
CHECK-NEXT: function u0:0(i32, i64) -> i32 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; [null-ended-array: program-parameter-names]
CHECK-NEXT:     ss1 = explicit_slot 8 ; [null-ended-array: global-files]
CHECK-NEXT:     gv0 = symbol colocated userextname4 ; [input-textfile]
CHECK-NEXT:     gv1 = symbol colocated userextname5 ; [output-textfile]
CHECK-NEXT:     gv2 = symbol colocated userextname6 ; a
CHECK-NEXT:     sig0 = (i32, i64, i32, i64, i32, i64) system_v
CHECK-NEXT:     sig1 = (i32, i64) system_v
CHECK-NEXT:     sig2 = () -> i64 system_v
CHECK-NEXT:     sig3 = () -> i64 system_v
CHECK-NEXT:     fn0 = u0:1 sig0 ; __pasko_init
CHECK-NEXT:     fn1 = u0:2 sig1 ; __pasko_finish
CHECK-NEXT:     fn2 = u0:3 sig2 ; __pasko_get_input
CHECK-NEXT:     fn3 = u0:4 sig3 ; __pasko_get_output
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
CHECK-NEXT:     v15 = iconst.i64 80
CHECK-NEXT:     v16 = iconst.i64 1
CHECK-NEXT:     v17 = isub v16, v14  ; v16 = 1, v14 = 1
CHECK-NEXT:     v18 = imul v17, v15  ; v15 = 80
CHECK-NEXT:     v19 = iadd v13, v18
CHECK-NEXT:     v20 = iconst.i64 10
CHECK-NEXT:     v21 = iconst.i64 8
CHECK-NEXT:     v22 = iconst.i64 10
CHECK-NEXT:     v23 = isub v22, v20  ; v22 = 10, v20 = 10
CHECK-NEXT:     v24 = imul v23, v21  ; v21 = 8
CHECK-NEXT:     v25 = iadd v19, v24
CHECK-NEXT:     store v12, v25  ; v12 = 3
CHECK-NEXT:     call fn1(v7, v6)  ; v7 = 0
CHECK-NEXT:     v26 = iconst.i32 0
CHECK-NEXT:     return v26  ; v26 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'main' seems OK

}

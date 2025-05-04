{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program test;

type
  t = set of integer;

var
  a, b : t;
  x: boolean;

begin
  a := [1, 2, 3];
  b := [4, 5, 6];

  x := a = b;
  x := a <> b;
  x := a >= b;
  x := a <= b;
end.

{

CHECK: *** IR for 'main'
CHECK-NEXT: function u0:0(i32, i64) -> i32 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8, align = 8 ; [null-ended-array: program-parameter-names]
CHECK-NEXT:     ss1 = explicit_slot 8, align = 8 ; [null-ended-array: global-files]
CHECK-NEXT:     ss2 = explicit_slot 24, align = 8 ; [set-constructor]
CHECK-NEXT:     ss3 = explicit_slot 24, align = 8 ; [set-constructor]
CHECK-NEXT:     gv0 = symbol colocated userextname4 ; [input-textfile]
CHECK-NEXT:     gv1 = symbol colocated userextname5 ; [output-textfile]
CHECK-NEXT:     gv2 = symbol colocated userextname7 ; a
CHECK-NEXT:     gv3 = symbol colocated userextname9 ; b
CHECK-NEXT:     gv4 = symbol colocated userextname11 ; x
CHECK-NEXT:     sig0 = (i32, i64, i32, i64, i32, i64) system_v
CHECK-NEXT:     sig1 = (i32, i64) system_v
CHECK-NEXT:     sig2 = () -> i64 system_v
CHECK-NEXT:     sig3 = () -> i64 system_v
CHECK-NEXT:     sig4 = (i64, i64) -> i64 system_v
CHECK-NEXT:     sig5 = (i64) system_v
CHECK-NEXT:     sig6 = (i64, i64) -> i8 system_v
CHECK-NEXT:     sig7 = (i64, i64) -> i8 system_v
CHECK-NEXT:     sig8 = (i64, i64) -> i8 system_v
CHECK-NEXT:     fn0 = u0:1 sig0 ; __pasko_init
CHECK-NEXT:     fn1 = u0:2 sig1 ; __pasko_finish
CHECK-NEXT:     fn2 = u0:3 sig2 ; __pasko_get_input
CHECK-NEXT:     fn3 = u0:4 sig3 ; __pasko_get_output
CHECK-NEXT:     fn4 = u0:5 sig4 ; __pasko_set_new
CHECK-NEXT:     fn5 = u0:6 sig5 ; __pasko_set_dispose
CHECK-NEXT:     fn6 = u0:7 sig6 ; __pasko_set_equal
CHECK-NEXT:     fn7 = u0:8 sig7 ; __pasko_set_not_equal
CHECK-NEXT:     fn8 = u0:9 sig8 ; __pasko_set_is_subset
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
CHECK-NEXT:     v12 = iconst.i64 1
CHECK-NEXT:     v13 = iconst.i64 2
CHECK-NEXT:     v14 = iconst.i64 3
CHECK-NEXT:     stack_store v12, ss2  ; v12 = 1
CHECK-NEXT:     stack_store v13, ss2+8  ; v13 = 2
CHECK-NEXT:     stack_store v14, ss2+16  ; v14 = 3
CHECK-NEXT:     v15 = iconst.i64 3
CHECK-NEXT:     v16 = stack_addr.i64 ss2
CHECK-NEXT:     v17 = call fn4(v15, v16)  ; v15 = 3
CHECK-NEXT:     v18 = global_value.i64 gv2
CHECK-NEXT:     v19 = load.i64 v18
CHECK-NEXT:     call fn5(v19)
CHECK-NEXT:     store v17, v18
CHECK-NEXT:     v20 = iconst.i64 4
CHECK-NEXT:     v21 = iconst.i64 5
CHECK-NEXT:     v22 = iconst.i64 6
CHECK-NEXT:     stack_store v20, ss3  ; v20 = 4
CHECK-NEXT:     stack_store v21, ss3+8  ; v21 = 5
CHECK-NEXT:     stack_store v22, ss3+16  ; v22 = 6
CHECK-NEXT:     v23 = iconst.i64 3
CHECK-NEXT:     v24 = stack_addr.i64 ss3
CHECK-NEXT:     v25 = call fn4(v23, v24)  ; v23 = 3
CHECK-NEXT:     v26 = global_value.i64 gv3
CHECK-NEXT:     v27 = load.i64 v26
CHECK-NEXT:     call fn5(v27)
CHECK-NEXT:     store v25, v26
CHECK-NEXT:     v28 = global_value.i64 gv2
CHECK-NEXT:     v29 = load.i64 v28
CHECK-NEXT:     v30 = global_value.i64 gv3
CHECK-NEXT:     v31 = load.i64 v30
CHECK-NEXT:     v32 = call fn6(v29, v31)
CHECK-NEXT:     v33 = global_value.i64 gv4
CHECK-NEXT:     store v32, v33
CHECK-NEXT:     v34 = global_value.i64 gv2
CHECK-NEXT:     v35 = load.i64 v34
CHECK-NEXT:     v36 = global_value.i64 gv3
CHECK-NEXT:     v37 = load.i64 v36
CHECK-NEXT:     v38 = call fn7(v35, v37)
CHECK-NEXT:     v39 = global_value.i64 gv4
CHECK-NEXT:     store v38, v39
CHECK-NEXT:     v40 = global_value.i64 gv2
CHECK-NEXT:     v41 = load.i64 v40
CHECK-NEXT:     v42 = global_value.i64 gv3
CHECK-NEXT:     v43 = load.i64 v42
CHECK-NEXT:     v44 = call fn8(v43, v41)
CHECK-NEXT:     v45 = global_value.i64 gv4
CHECK-NEXT:     store v44, v45
CHECK-NEXT:     v46 = global_value.i64 gv2
CHECK-NEXT:     v47 = load.i64 v46
CHECK-NEXT:     v48 = global_value.i64 gv3
CHECK-NEXT:     v49 = load.i64 v48
CHECK-NEXT:     v50 = call fn8(v47, v49)
CHECK-NEXT:     v51 = global_value.i64 gv4
CHECK-NEXT:     store v50, v51
CHECK-NEXT:     v52 = global_value.i64 gv2
CHECK-NEXT:     v53 = load.i64 v52
CHECK-NEXT:     call fn5(v53)
CHECK-NEXT:     v54 = global_value.i64 gv3
CHECK-NEXT:     v55 = load.i64 v54
CHECK-NEXT:     call fn5(v55)
CHECK-NEXT:     call fn1(v7, v6)  ; v7 = 0
CHECK-NEXT:     v56 = iconst.i32 0
CHECK-NEXT:     return v56  ; v56 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'main' seems OK

}

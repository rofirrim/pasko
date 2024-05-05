{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program test;

type
  t = set of integer;

var
  a, b, c : t;
  x: boolean;

begin
  a := [1, 2, 3];
  b := [4, 5, 6];

  c := a;

  c := a + b;
  c := a - b;
  c := a * b;

  x := 3 in a;
end.

{

CHECK-LABEL: *** IR for main
CHECK: function u0:19(i32, i64) -> i32 system_v {
CHECK:     ss0 = explicit_slot 24
CHECK:     ss1 = explicit_slot 24
CHECK:     gv0 = symbol colocated userextname1
CHECK:     gv1 = symbol colocated userextname3
CHECK:     gv2 = symbol colocated userextname4
CHECK:     gv3 = symbol colocated userextname10
CHECK:     sig0 = (i64, i64) -> i64 system_v
CHECK:     sig1 = (i64) system_v
CHECK:     sig2 = (i64) -> i64 system_v
CHECK:     sig3 = (i64, i64) -> i64 system_v
CHECK:     sig4 = (i64, i64) -> i64 system_v
CHECK:     sig5 = (i64, i64) -> i64 system_v
CHECK:     sig6 = (i64, i64) -> i8 system_v
CHECK:     fn0 = u0:9 sig0
CHECK:     fn1 = u0:10 sig1
CHECK:     fn2 = u0:11 sig2
CHECK:     fn3 = u0:12 sig3
CHECK:     fn4 = u0:14 sig4
CHECK:     fn5 = u0:13 sig5
CHECK:     fn6 = u0:15 sig6
CHECK-EMPTY:
CHECK: block0(v0: i32, v1: i64):
CHECK:     v2 = iconst.i64 1
CHECK:     v3 = iconst.i64 2
CHECK:     v4 = iconst.i64 3
CHECK:     v5 = stack_addr.i64 ss0
CHECK:     store v2, v5  ; v2 = 1
CHECK:     store v3, v5+8  ; v3 = 2
CHECK:     store v4, v5+16  ; v4 = 3
CHECK:     v6 = iconst.i64 3
CHECK:     v7 = call fn0(v6, v5)  ; v6 = 3
CHECK:     v8 = global_value.i64 gv0
CHECK:     v9 = load.i64 v8
CHECK:     call fn1(v9)
CHECK:     store v7, v8
CHECK:     v10 = iconst.i64 4
CHECK:     v11 = iconst.i64 5
CHECK:     v12 = iconst.i64 6
CHECK:     v13 = stack_addr.i64 ss1
CHECK:     store v10, v13  ; v10 = 4
CHECK:     store v11, v13+8  ; v11 = 5
CHECK:     store v12, v13+16  ; v12 = 6
CHECK:     v14 = iconst.i64 3
CHECK:     v15 = call fn0(v14, v13)  ; v14 = 3
CHECK:     v16 = global_value.i64 gv1
CHECK:     v17 = load.i64 v16
CHECK:     call fn1(v17)
CHECK:     store v15, v16
CHECK:     v18 = global_value.i64 gv0
CHECK:     v19 = load.i64 v18
CHECK:     v20 = global_value.i64 gv2
CHECK:     v21 = load.i64 v20
CHECK:     call fn1(v21)
CHECK:     v22 = call fn2(v19)
CHECK:     store v22, v20
CHECK:     v23 = global_value.i64 gv0
CHECK:     v24 = load.i64 v23
CHECK:     v25 = global_value.i64 gv1
CHECK:     v26 = load.i64 v25
CHECK:     v27 = call fn3(v24, v26)
CHECK:     v28 = global_value.i64 gv2
CHECK:     v29 = load.i64 v28
CHECK:     call fn1(v29)
CHECK:     store v27, v28
CHECK:     v30 = global_value.i64 gv0
CHECK:     v31 = load.i64 v30
CHECK:     v32 = global_value.i64 gv1
CHECK:     v33 = load.i64 v32
CHECK:     v34 = call fn4(v31, v33)
CHECK:     v35 = global_value.i64 gv2
CHECK:     v36 = load.i64 v35
CHECK:     call fn1(v36)
CHECK:     store v34, v35
CHECK:     v37 = global_value.i64 gv0
CHECK:     v38 = load.i64 v37
CHECK:     v39 = global_value.i64 gv1
CHECK:     v40 = load.i64 v39
CHECK:     v41 = call fn5(v38, v40)
CHECK:     v42 = global_value.i64 gv2
CHECK:     v43 = load.i64 v42
CHECK:     call fn1(v43)
CHECK:     store v41, v42
CHECK:     v44 = iconst.i64 3
CHECK:     v45 = global_value.i64 gv0
CHECK:     v46 = load.i64 v45
CHECK:     v47 = call fn6(v46, v44)  ; v44 = 3
CHECK:     v48 = global_value.i64 gv3
CHECK:     store v47, v48
CHECK:     v49 = global_value.i64 gv0
CHECK:     v50 = load.i64 v49
CHECK:     call fn1(v50)
CHECK:     v51 = global_value.i64 gv1
CHECK:     v52 = load.i64 v51
CHECK:     call fn1(v52)
CHECK:     v53 = global_value.i64 gv2
CHECK:     v54 = load.i64 v53
CHECK:     call fn1(v54)
CHECK:     v55 = iconst.i32 0
CHECK:     return v55  ; v55 = 0
CHECK: }
CHECK: *** IR for main seems OK

}

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

CHECK: *** IR for main
CHECK-NEXT: function u0:0(i32, i64) -> i32 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-NEXT:     ss2 = explicit_slot 24
CHECK-NEXT:     ss3 = explicit_slot 24
CHECK-NEXT:     gv0 = symbol colocated userextname4
CHECK-NEXT:     gv1 = symbol colocated userextname5
CHECK-NEXT:     gv2 = symbol colocated userextname7
CHECK-NEXT:     gv3 = symbol colocated userextname9
CHECK-NEXT:     gv4 = symbol colocated userextname10
CHECK-NEXT:     gv5 = symbol colocated userextname16
CHECK-NEXT:     sig0 = (i32, i64, i32, i64, i32, i64) system_v
CHECK-NEXT:     sig1 = (i32, i64) system_v
CHECK-NEXT:     sig2 = () -> i64 system_v
CHECK-NEXT:     sig3 = () -> i64 system_v
CHECK-NEXT:     sig4 = (i64, i64) -> i64 system_v
CHECK-NEXT:     sig5 = (i64) system_v
CHECK-NEXT:     sig6 = (i64) -> i64 system_v
CHECK-NEXT:     sig7 = (i64, i64) -> i64 system_v
CHECK-NEXT:     sig8 = (i64, i64) -> i64 system_v
CHECK-NEXT:     sig9 = (i64, i64) -> i64 system_v
CHECK-NEXT:     sig10 = (i64, i64) -> i8 system_v
CHECK-NEXT:     fn0 = u0:1 sig0
CHECK-NEXT:     fn1 = u0:2 sig1
CHECK-NEXT:     fn2 = u0:3 sig2
CHECK-NEXT:     fn3 = u0:4 sig3
CHECK-NEXT:     fn4 = u0:5 sig4
CHECK-NEXT:     fn5 = u0:6 sig5
CHECK-NEXT:     fn6 = u0:7 sig6
CHECK-NEXT:     fn7 = u0:8 sig7
CHECK-NEXT:     fn8 = u0:9 sig8
CHECK-NEXT:     fn9 = u0:10 sig9
CHECK-NEXT:     fn10 = u0:11 sig10
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
CHECK-NEXT:     v15 = stack_addr.i64 ss2
CHECK-NEXT:     store v12, v15  ; v12 = 1
CHECK-NEXT:     store v13, v15+8  ; v13 = 2
CHECK-NEXT:     store v14, v15+16  ; v14 = 3
CHECK-NEXT:     v16 = iconst.i64 3
CHECK-NEXT:     v17 = call fn4(v16, v15)  ; v16 = 3
CHECK-NEXT:     v18 = global_value.i64 gv2
CHECK-NEXT:     v19 = load.i64 v18
CHECK-NEXT:     call fn5(v19)
CHECK-NEXT:     store v17, v18
CHECK-NEXT:     v20 = iconst.i64 4
CHECK-NEXT:     v21 = iconst.i64 5
CHECK-NEXT:     v22 = iconst.i64 6
CHECK-NEXT:     v23 = stack_addr.i64 ss3
CHECK-NEXT:     store v20, v23  ; v20 = 4
CHECK-NEXT:     store v21, v23+8  ; v21 = 5
CHECK-NEXT:     store v22, v23+16  ; v22 = 6
CHECK-NEXT:     v24 = iconst.i64 3
CHECK-NEXT:     v25 = call fn4(v24, v23)  ; v24 = 3
CHECK-NEXT:     v26 = global_value.i64 gv3
CHECK-NEXT:     v27 = load.i64 v26
CHECK-NEXT:     call fn5(v27)
CHECK-NEXT:     store v25, v26
CHECK-NEXT:     v28 = global_value.i64 gv2
CHECK-NEXT:     v29 = load.i64 v28
CHECK-NEXT:     v30 = global_value.i64 gv4
CHECK-NEXT:     v31 = load.i64 v30
CHECK-NEXT:     call fn5(v31)
CHECK-NEXT:     v32 = call fn6(v29)
CHECK-NEXT:     store v32, v30
CHECK-NEXT:     v33 = global_value.i64 gv2
CHECK-NEXT:     v34 = load.i64 v33
CHECK-NEXT:     v35 = global_value.i64 gv3
CHECK-NEXT:     v36 = load.i64 v35
CHECK-NEXT:     v37 = call fn7(v34, v36)
CHECK-NEXT:     v38 = global_value.i64 gv4
CHECK-NEXT:     v39 = load.i64 v38
CHECK-NEXT:     call fn5(v39)
CHECK-NEXT:     store v37, v38
CHECK-NEXT:     v40 = global_value.i64 gv2
CHECK-NEXT:     v41 = load.i64 v40
CHECK-NEXT:     v42 = global_value.i64 gv3
CHECK-NEXT:     v43 = load.i64 v42
CHECK-NEXT:     v44 = call fn8(v41, v43)
CHECK-NEXT:     v45 = global_value.i64 gv4
CHECK-NEXT:     v46 = load.i64 v45
CHECK-NEXT:     call fn5(v46)
CHECK-NEXT:     store v44, v45
CHECK-NEXT:     v47 = global_value.i64 gv2
CHECK-NEXT:     v48 = load.i64 v47
CHECK-NEXT:     v49 = global_value.i64 gv3
CHECK-NEXT:     v50 = load.i64 v49
CHECK-NEXT:     v51 = call fn9(v48, v50)
CHECK-NEXT:     v52 = global_value.i64 gv4
CHECK-NEXT:     v53 = load.i64 v52
CHECK-NEXT:     call fn5(v53)
CHECK-NEXT:     store v51, v52
CHECK-NEXT:     v54 = iconst.i64 3
CHECK-NEXT:     v55 = global_value.i64 gv2
CHECK-NEXT:     v56 = load.i64 v55
CHECK-NEXT:     v57 = call fn10(v56, v54)  ; v54 = 3
CHECK-NEXT:     v58 = global_value.i64 gv5
CHECK-NEXT:     store v57, v58
CHECK-NEXT:     v59 = global_value.i64 gv2
CHECK-NEXT:     v60 = load.i64 v59
CHECK-NEXT:     call fn5(v60)
CHECK-NEXT:     v61 = global_value.i64 gv3
CHECK-NEXT:     v62 = load.i64 v61
CHECK-NEXT:     call fn5(v62)
CHECK-NEXT:     v63 = global_value.i64 gv4
CHECK-NEXT:     v64 = load.i64 v63
CHECK-NEXT:     call fn5(v64)
CHECK-NEXT:     call fn1(v7, v6)  ; v7 = 0
CHECK-NEXT:     v65 = iconst.i32 0
CHECK-NEXT:     return v65  ; v65 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for main seems OK

}

{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main(output);

begin
  writeln(true);
  writeln(false);

  writeln('hello');

  writeln(1);
  writeln(1:10);

  writeln(1.23);
  writeln(1.23:10);
  writeln(1.23:10:3);
end.

{

CHECK: *** IR for main
CHECK-NEXT: function u0:38(i32, i64) -> i32 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-NEXT:     gv0 = symbol colocated userextname4
CHECK-NEXT:     gv1 = symbol colocated userextname5
CHECK-NEXT:     gv2 = symbol colocated userextname8
CHECK-NEXT:     sig0 = (i32, i64, i32, i64, i32, i64) system_v
CHECK-NEXT:     sig1 = (i32, i64) system_v
CHECK-NEXT:     sig2 = () -> i64 system_v
CHECK-NEXT:     sig3 = () -> i64 system_v
CHECK-NEXT:     sig4 = (i64, i8) system_v
CHECK-NEXT:     sig5 = (i64) system_v
CHECK-NEXT:     sig6 = (i64, i64) system_v
CHECK-NEXT:     sig7 = (i64, i64, i64) system_v
CHECK-NEXT:     sig8 = (i64, f64, i64, i64) system_v
CHECK-NEXT:     fn0 = u0:21 sig0
CHECK-NEXT:     fn1 = u0:22 sig1
CHECK-NEXT:     fn2 = u0:23 sig2
CHECK-NEXT:     fn3 = u0:24 sig3
CHECK-NEXT:     fn4 = u0:3 sig4
CHECK-NEXT:     fn5 = u0:5 sig5
CHECK-NEXT:     fn6 = u0:0 sig6
CHECK-NEXT:     fn7 = u0:1 sig7
CHECK-NEXT:     fn8 = u0:2 sig8
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
CHECK-NEXT:     v12 = global_value.i64 gv1
CHECK-NEXT:     v13 = load.i64 v12
CHECK-NEXT:     v14 = iconst.i8 1
CHECK-NEXT:     call fn4(v13, v14)  ; v14 = 1
CHECK-NEXT:     call fn5(v13)
CHECK-NEXT:     v15 = global_value.i64 gv1
CHECK-NEXT:     v16 = load.i64 v15
CHECK-NEXT:     v17 = iconst.i8 0
CHECK-NEXT:     call fn4(v16, v17)  ; v17 = 0
CHECK-NEXT:     call fn5(v16)
CHECK-NEXT:     v18 = global_value.i64 gv1
CHECK-NEXT:     v19 = load.i64 v18
CHECK-NEXT:     v20 = global_value.i64 gv2
CHECK-NEXT:     call fn6(v19, v20)
CHECK-NEXT:     call fn5(v19)
CHECK-NEXT:     v21 = global_value.i64 gv1
CHECK-NEXT:     v22 = load.i64 v21
CHECK-NEXT:     v23 = iconst.i64 1
CHECK-NEXT:     v24 = iconst.i64 0
CHECK-NEXT:     call fn7(v22, v23, v24)  ; v23 = 1, v24 = 0
CHECK-NEXT:     call fn5(v22)
CHECK-NEXT:     v25 = global_value.i64 gv1
CHECK-NEXT:     v26 = load.i64 v25
CHECK-NEXT:     v27 = iconst.i64 1
CHECK-NEXT:     v28 = iconst.i64 10
CHECK-NEXT:     call fn7(v26, v27, v28)  ; v27 = 1, v28 = 10
CHECK-NEXT:     call fn5(v26)
CHECK-NEXT:     v29 = global_value.i64 gv1
CHECK-NEXT:     v30 = load.i64 v29
CHECK-NEXT:     v31 = f64const 0x1.3ae147ae147aep0
CHECK-NEXT:     v32 = iconst.i64 0
CHECK-NEXT:     v33 = iconst.i64 0
CHECK-NEXT:     call fn8(v30, v31, v32, v33)  ; v31 = 0x1.3ae147ae147aep0, v32 = 0, v33 = 0
CHECK-NEXT:     call fn5(v30)
CHECK-NEXT:     v34 = global_value.i64 gv1
CHECK-NEXT:     v35 = load.i64 v34
CHECK-NEXT:     v36 = f64const 0x1.3ae147ae147aep0
CHECK-NEXT:     v37 = iconst.i64 10
CHECK-NEXT:     v38 = iconst.i64 0
CHECK-NEXT:     call fn8(v35, v36, v37, v38)  ; v36 = 0x1.3ae147ae147aep0, v37 = 10, v38 = 0
CHECK-NEXT:     call fn5(v35)
CHECK-NEXT:     v39 = global_value.i64 gv1
CHECK-NEXT:     v40 = load.i64 v39
CHECK-NEXT:     v41 = f64const 0x1.3ae147ae147aep0
CHECK-NEXT:     v42 = iconst.i64 10
CHECK-NEXT:     v43 = iconst.i64 3
CHECK-NEXT:     call fn8(v40, v41, v42, v43)  ; v41 = 0x1.3ae147ae147aep0, v42 = 10, v43 = 3
CHECK-NEXT:     call fn5(v40)
CHECK-NEXT:     call fn1(v7, v6)  ; v7 = 0
CHECK-NEXT:     v44 = iconst.i32 0
CHECK-NEXT:     return v44  ; v44 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for main seems OK

}

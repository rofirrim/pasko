{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main(my_out);

var
  my_out: text;

begin
  write(my_out, true);
  write(my_out, false);

  write(my_out, 'hello');

  write(my_out, 1);
  write(my_out, 1:10);

  write(my_out, 1.23);
  write(my_out, 1.23:10);
  write(my_out, 1.23:10:3);
end.

{

CHECK: *** IR for 'main'
CHECK-NEXT: function u0:0(i32, i64) -> i32 system_v {
CHECK-NEXT:     ss0 = explicit_slot 16
CHECK-NEXT:     ss1 = explicit_slot 16
CHECK-NEXT:     gv0 = symbol colocated userextname4
CHECK-NEXT:     gv1 = symbol colocated userextname5
CHECK-NEXT:     gv2 = symbol colocated userextname6
CHECK-NEXT:     gv3 = symbol colocated userextname7
CHECK-NEXT:     gv4 = symbol colocated userextname9
CHECK-NEXT:     sig0 = (i32, i64, i32, i64, i32, i64) system_v
CHECK-NEXT:     sig1 = (i32, i64) system_v
CHECK-NEXT:     sig2 = () -> i64 system_v
CHECK-NEXT:     sig3 = () -> i64 system_v
CHECK-NEXT:     sig4 = (i64, i8) system_v
CHECK-NEXT:     sig5 = (i64, i64) system_v
CHECK-NEXT:     sig6 = (i64, i64, i64) system_v
CHECK-NEXT:     sig7 = (i64, f64, i64, i64) system_v
CHECK-NEXT:     fn0 = u0:1 sig0
CHECK-NEXT:     fn1 = u0:2 sig1
CHECK-NEXT:     fn2 = u0:3 sig2
CHECK-NEXT:     fn3 = u0:4 sig3
CHECK-NEXT:     fn4 = u0:5 sig4
CHECK-NEXT:     fn5 = u0:6 sig5
CHECK-NEXT:     fn6 = u0:7 sig6
CHECK-NEXT:     fn7 = u0:8 sig7
CHECK-EMPTY: 
CHECK-NEXT: block0(v0: i32, v1: i64):
CHECK-NEXT:     v2 = iconst.i32 1
CHECK-NEXT:     v3 = global_value.i64 gv0
CHECK-NEXT:     stack_store v3, ss0
CHECK-NEXT:     v4 = iconst.i64 0
CHECK-NEXT:     stack_store v4, ss0+8  ; v4 = 0
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     v6 = global_value.i64 gv1
CHECK-NEXT:     stack_store v6, ss1
CHECK-NEXT:     v7 = iconst.i64 0
CHECK-NEXT:     stack_store v7, ss1+8  ; v7 = 0
CHECK-NEXT:     v8 = stack_addr.i64 ss1
CHECK-NEXT:     v9 = iconst.i32 1
CHECK-NEXT:     call fn0(v0, v1, v2, v5, v9, v8)  ; v2 = 1, v9 = 1
CHECK-NEXT:     v10 = call fn2()
CHECK-NEXT:     v11 = global_value.i64 gv2
CHECK-NEXT:     store v10, v11
CHECK-NEXT:     v12 = call fn3()
CHECK-NEXT:     v13 = global_value.i64 gv3
CHECK-NEXT:     store v12, v13
CHECK-NEXT:     v14 = global_value.i64 gv1
CHECK-NEXT:     v15 = load.i64 v14
CHECK-NEXT:     v16 = iconst.i8 1
CHECK-NEXT:     call fn4(v15, v16)  ; v16 = 1
CHECK-NEXT:     v17 = global_value.i64 gv1
CHECK-NEXT:     v18 = load.i64 v17
CHECK-NEXT:     v19 = iconst.i8 0
CHECK-NEXT:     call fn4(v18, v19)  ; v19 = 0
CHECK-NEXT:     v20 = global_value.i64 gv1
CHECK-NEXT:     v21 = load.i64 v20
CHECK-NEXT:     v22 = global_value.i64 gv4
CHECK-NEXT:     call fn5(v21, v22)
CHECK-NEXT:     v23 = global_value.i64 gv1
CHECK-NEXT:     v24 = load.i64 v23
CHECK-NEXT:     v25 = iconst.i64 1
CHECK-NEXT:     v26 = iconst.i64 0
CHECK-NEXT:     call fn6(v24, v25, v26)  ; v25 = 1, v26 = 0
CHECK-NEXT:     v27 = global_value.i64 gv1
CHECK-NEXT:     v28 = load.i64 v27
CHECK-NEXT:     v29 = iconst.i64 1
CHECK-NEXT:     v30 = iconst.i64 10
CHECK-NEXT:     call fn6(v28, v29, v30)  ; v29 = 1, v30 = 10
CHECK-NEXT:     v31 = global_value.i64 gv1
CHECK-NEXT:     v32 = load.i64 v31
CHECK-NEXT:     v33 = f64const 0x1.3ae147ae147aep0
CHECK-NEXT:     v34 = iconst.i64 0
CHECK-NEXT:     v35 = iconst.i64 0
CHECK-NEXT:     call fn7(v32, v33, v34, v35)  ; v33 = 0x1.3ae147ae147aep0, v34 = 0, v35 = 0
CHECK-NEXT:     v36 = global_value.i64 gv1
CHECK-NEXT:     v37 = load.i64 v36
CHECK-NEXT:     v38 = f64const 0x1.3ae147ae147aep0
CHECK-NEXT:     v39 = iconst.i64 10
CHECK-NEXT:     v40 = iconst.i64 0
CHECK-NEXT:     call fn7(v37, v38, v39, v40)  ; v38 = 0x1.3ae147ae147aep0, v39 = 10, v40 = 0
CHECK-NEXT:     v41 = global_value.i64 gv1
CHECK-NEXT:     v42 = load.i64 v41
CHECK-NEXT:     v43 = f64const 0x1.3ae147ae147aep0
CHECK-NEXT:     v44 = iconst.i64 10
CHECK-NEXT:     v45 = iconst.i64 3
CHECK-NEXT:     call fn7(v42, v43, v44, v45)  ; v43 = 0x1.3ae147ae147aep0, v44 = 10, v45 = 3
CHECK-NEXT:     call fn1(v9, v8)  ; v9 = 1
CHECK-NEXT:     v46 = iconst.i32 0
CHECK-NEXT:     return v46  ; v46 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'main' seems OK

}

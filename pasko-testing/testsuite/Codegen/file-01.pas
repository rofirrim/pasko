{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main(f);

var
  f: file of real;
  x: real;

begin
  rewrite(f);
  write(f, 123.456);

  reset(f);
  read(f, x);
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
CHECK-NEXT:     gv4 = symbol colocated userextname12
CHECK-NEXT:     sig0 = (i32, i64, i32, i64, i32, i64) system_v
CHECK-NEXT:     sig1 = (i32, i64) system_v
CHECK-NEXT:     sig2 = () -> i64 system_v
CHECK-NEXT:     sig3 = () -> i64 system_v
CHECK-NEXT:     sig4 = (i64) system_v
CHECK-NEXT:     sig5 = (i64, i64) -> i64 system_v
CHECK-NEXT:     sig6 = (i64, i64) system_v
CHECK-NEXT:     sig7 = (i64, i64) system_v
CHECK-NEXT:     sig8 = (i64, i64) system_v
CHECK-NEXT:     fn0 = u0:1 sig0
CHECK-NEXT:     fn1 = u0:2 sig1
CHECK-NEXT:     fn2 = u0:3 sig2
CHECK-NEXT:     fn3 = u0:4 sig3
CHECK-NEXT:     fn4 = u0:5 sig4
CHECK-NEXT:     fn5 = u0:6 sig5
CHECK-NEXT:     fn6 = u0:7 sig6
CHECK-NEXT:     fn7 = u0:8 sig7
CHECK-NEXT:     fn8 = u0:9 sig8
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
CHECK-NEXT:     call fn4(v15)
CHECK-NEXT:     v16 = global_value.i64 gv1
CHECK-NEXT:     v17 = load.i64 v16
CHECK-NEXT:     v18 = f64const 0x1.edd2f1a9fbe77p6
CHECK-NEXT:     v19 = iconst.i64 8
CHECK-NEXT:     v20 = call fn5(v17, v19)  ; v19 = 8
CHECK-NEXT:     store v18, v20  ; v18 = 0x1.edd2f1a9fbe77p6
CHECK-NEXT:     call fn6(v17, v19)  ; v19 = 8
CHECK-NEXT:     v21 = global_value.i64 gv1
CHECK-NEXT:     v22 = load.i64 v21
CHECK-NEXT:     v23 = iconst.i64 8
CHECK-NEXT:     call fn7(v22, v23)  ; v23 = 8
CHECK-NEXT:     v24 = global_value.i64 gv1
CHECK-NEXT:     v25 = load.i64 v24
CHECK-NEXT:     v26 = global_value.i64 gv4
CHECK-NEXT:     v27 = iconst.i64 8
CHECK-NEXT:     v28 = call fn5(v25, v27)  ; v27 = 8
CHECK-NEXT:     v29 = load.f64 v28
CHECK-NEXT:     store v29, v26
CHECK-NEXT:     call fn8(v25, v27)  ; v27 = 8
CHECK-NEXT:     call fn1(v9, v8)  ; v9 = 1
CHECK-NEXT:     v30 = iconst.i32 0
CHECK-NEXT:     return v30  ; v30 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'main' seems OK

}

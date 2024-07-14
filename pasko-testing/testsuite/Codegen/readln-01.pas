{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main(input);

var
   x: integer;
   y: real;
begin
  readln(x);
  readln(y);
end.

{

CHECK: *** IR for main
CHECK-NEXT: function u0:38(i32, i64) -> i32 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-NEXT:     gv0 = symbol colocated userextname4
CHECK-NEXT:     gv1 = symbol colocated userextname5
CHECK-NEXT:     gv2 = symbol colocated userextname6
CHECK-NEXT:     gv3 = symbol colocated userextname9
CHECK-NEXT:     sig0 = (i32, i64, i32, i64, i32, i64) system_v
CHECK-NEXT:     sig1 = (i32, i64) system_v
CHECK-NEXT:     sig2 = () -> i64 system_v
CHECK-NEXT:     sig3 = () -> i64 system_v
CHECK-NEXT:     sig4 = (i64) -> i64 system_v
CHECK-NEXT:     sig5 = (i64) system_v
CHECK-NEXT:     sig6 = (i64) -> f64 system_v
CHECK-NEXT:     fn0 = u0:21 sig0
CHECK-NEXT:     fn1 = u0:22 sig1
CHECK-NEXT:     fn2 = u0:23 sig2
CHECK-NEXT:     fn3 = u0:24 sig3
CHECK-NEXT:     fn4 = u0:6 sig4
CHECK-NEXT:     fn5 = u0:8 sig5
CHECK-NEXT:     fn6 = u0:7 sig6
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
CHECK-NEXT:     v12 = global_value.i64 gv0
CHECK-NEXT:     v13 = load.i64 v12
CHECK-NEXT:     v14 = global_value.i64 gv2
CHECK-NEXT:     v15 = call fn4(v13)
CHECK-NEXT:     store v15, v14
CHECK-NEXT:     call fn5(v13)
CHECK-NEXT:     v16 = global_value.i64 gv0
CHECK-NEXT:     v17 = load.i64 v16
CHECK-NEXT:     v18 = global_value.i64 gv3
CHECK-NEXT:     v19 = call fn6(v17)
CHECK-NEXT:     store v19, v18
CHECK-NEXT:     call fn5(v17)
CHECK-NEXT:     call fn1(v7, v6)  ; v7 = 0
CHECK-NEXT:     v20 = iconst.i32 0
CHECK-NEXT:     return v20  ; v20 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for main seems OK

}

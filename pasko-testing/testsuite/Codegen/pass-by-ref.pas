{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main;
var
  z : integer;

procedure increment(n: integer; var x: integer);
var
  m : integer;
begin
    x := 3;
    increment(n, m);
end;

begin
  z := 3;
  increment(1, z);
end.

{

CHECK: *** IR for 'increment'
CHECK-NEXT: function u0:0(i64, i64) system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-NEXT:     ss2 = explicit_slot 8
CHECK-NEXT:     sig0 = (i64, i64) system_v
CHECK-NEXT:     fn0 = colocated u0:0 sig0
CHECK-EMPTY: 
CHECK-NEXT: block0(v0: i64, v1: i64):
CHECK-NEXT:     v2 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v2
CHECK-NEXT:     v3 = stack_addr.i64 ss1
CHECK-NEXT:     store v1, v3
CHECK-NEXT:     v4 = iconst.i64 3
CHECK-NEXT:     v5 = stack_addr.i64 ss1
CHECK-NEXT:     v6 = load.i64 v5
CHECK-NEXT:     store v4, v6  ; v4 = 3
CHECK-NEXT:     v7 = stack_addr.i64 ss0
CHECK-NEXT:     v8 = load.i64 v7
CHECK-NEXT:     v9 = stack_addr.i64 ss2
CHECK-NEXT:     call fn0(v8, v9)
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'increment' seems OK

CHECK: *** IR for 'main'
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
CHECK-NEXT:     sig4 = (i64, i64) system_v
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
CHECK-NEXT:     v12 = iconst.i64 3
CHECK-NEXT:     v13 = global_value.i64 gv2
CHECK-NEXT:     store v12, v13  ; v12 = 3
CHECK-NEXT:     v14 = iconst.i64 1
CHECK-NEXT:     v15 = global_value.i64 gv2
CHECK-NEXT:     call fn4(v14, v15)  ; v14 = 1
CHECK-NEXT:     call fn1(v7, v6)  ; v7 = 0
CHECK-NEXT:     v16 = iconst.i32 0
CHECK-NEXT:     return v16  ; v16 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'main' seems OK

}

{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program test;

type
  t = set of integer;

procedure foo_val(a: t);
begin
  a := [51];
end;

procedure foo_ref(var a: t);
begin
  a := [42];
end;

begin
end.

{

CHECK: *** IR for procedure 'foo_val'
CHECK-NEXT: function u0:0(i64) system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-NEXT:     sig0 = (i64, i64) -> i64 system_v
CHECK-NEXT:     sig1 = (i64) system_v
CHECK-NEXT:     fn0 = u0:1 sig0
CHECK-NEXT:     fn1 = u0:2 sig1
CHECK-EMPTY: 
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = iconst.i64 51
CHECK-NEXT:     v3 = stack_addr.i64 ss1
CHECK-NEXT:     store v2, v3  ; v2 = 51
CHECK-NEXT:     v4 = iconst.i64 1
CHECK-NEXT:     v5 = call fn0(v4, v3)  ; v4 = 1
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.i64 v6
CHECK-NEXT:     call fn1(v7)
CHECK-NEXT:     store v5, v6
CHECK-NEXT:     v8 = stack_addr.i64 ss0
CHECK-NEXT:     v9 = load.i64 v8
CHECK-NEXT:     call fn1(v9)
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for procedure 'foo_val' seems OK

CHECK: *** IR for procedure 'foo_ref'
CHECK-NEXT: function u0:3(i64) system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-NEXT:     sig0 = (i64, i64) -> i64 system_v
CHECK-NEXT:     sig1 = (i64) system_v
CHECK-NEXT:     fn0 = u0:1 sig0
CHECK-NEXT:     fn1 = u0:2 sig1
CHECK-EMPTY: 
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = iconst.i64 42
CHECK-NEXT:     v3 = stack_addr.i64 ss1
CHECK-NEXT:     store v2, v3  ; v2 = 42
CHECK-NEXT:     v4 = iconst.i64 1
CHECK-NEXT:     v5 = call fn0(v4, v3)  ; v4 = 1
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.i64 v6
CHECK-NEXT:     v8 = load.i64 v7
CHECK-NEXT:     call fn1(v8)
CHECK-NEXT:     store v5, v7
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for procedure 'foo_ref' seems OK

CHECK: *** IR for main
CHECK-NEXT: function u0:4(i32, i64) -> i32 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-NEXT:     gv0 = symbol colocated userextname4
CHECK-NEXT:     gv1 = symbol colocated userextname5
CHECK-NEXT:     sig0 = (i32, i64, i32, i64, i32, i64) system_v
CHECK-NEXT:     sig1 = (i32, i64) system_v
CHECK-NEXT:     sig2 = () -> i64 system_v
CHECK-NEXT:     sig3 = () -> i64 system_v
CHECK-NEXT:     fn0 = u0:5 sig0
CHECK-NEXT:     fn1 = u0:6 sig1
CHECK-NEXT:     fn2 = u0:7 sig2
CHECK-NEXT:     fn3 = u0:8 sig3
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
CHECK-NEXT:     call fn1(v7, v6)  ; v7 = 0
CHECK-NEXT:     v12 = iconst.i32 0
CHECK-NEXT:     return v12  ; v12 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for main seems OK

}

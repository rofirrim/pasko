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

CHECK-LABEL: *** IR for procedure 'foo_val'
CHECK: function u0:21(i64) system_v {
CHECK:     ss0 = explicit_slot 8
CHECK:     ss1 = explicit_slot 8
CHECK:     sig0 = (i64, i64) -> i64 system_v
CHECK:     sig1 = (i64) system_v
CHECK:     fn0 = u0:9 sig0
CHECK:     fn1 = u0:10 sig1
CHECK-EMPTY:
CHECK: block0(v0: i64):
CHECK:     v1 = stack_addr.i64 ss0
CHECK:     store v0, v1
CHECK:     v2 = iconst.i64 51
CHECK:     v3 = stack_addr.i64 ss1
CHECK:     store v2, v3  ; v2 = 51
CHECK:     v4 = iconst.i64 1
CHECK:     v5 = call fn0(v4, v3)  ; v4 = 1
CHECK:     v6 = stack_addr.i64 ss0
CHECK:     v7 = load.i64 v6
CHECK:     call fn1(v7)
CHECK:     store v5, v6
CHECK:     v8 = stack_addr.i64 ss0
CHECK:     v9 = load.i64 v8
CHECK:     call fn1(v9)
CHECK:     return
CHECK: }
CHECK: *** IR for procedure 'foo_val' seems OK

CHECK-LABEL: *** IR for procedure 'foo_ref'
CHECK: function u0:22(i64) system_v {
CHECK:     ss0 = explicit_slot 8
CHECK:     ss1 = explicit_slot 8
CHECK:     sig0 = (i64, i64) -> i64 system_v
CHECK:     sig1 = (i64) system_v
CHECK:     fn0 = u0:9 sig0
CHECK:     fn1 = u0:10 sig1
CHECK-EMPTY:
CHECK: block0(v0: i64):
CHECK:     v1 = stack_addr.i64 ss0
CHECK:     store v0, v1
CHECK:     v2 = iconst.i64 42
CHECK:     v3 = stack_addr.i64 ss1
CHECK:     store v2, v3  ; v2 = 42
CHECK:     v4 = iconst.i64 1
CHECK:     v5 = call fn0(v4, v3)  ; v4 = 1
CHECK:     v6 = stack_addr.i64 ss0
CHECK:     v7 = load.i64 v6
CHECK:     v8 = load.i64 v7
CHECK:     call fn1(v8)
CHECK:     store v5, v7
CHECK:     return
CHECK: }
CHECK: *** IR for procedure 'foo_ref' seems OK

CHECK-LABEL: *** IR for main
CHECK: function u0:23(i32, i64) -> i32 system_v {
CHECK: block0(v0: i32, v1: i64):
CHECK:     v2 = iconst.i32 0
CHECK:     return v2  ; v2 = 0
CHECK: }
CHECK: *** IR for main seems OK

}

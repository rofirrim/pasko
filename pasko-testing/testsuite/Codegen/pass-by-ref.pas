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

CHECK: *** IR for procedure 'increment'
CHECK-NEXT: function u0:9(i64, i64) system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-NEXT:     ss2 = explicit_slot 8
CHECK-NEXT:     sig0 = (i64, i64) system_v
CHECK-NEXT:     fn0 = colocated u0:9 sig0
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
CHECK-NEXT: *** IR for procedure 'increment' seems OK

CHECK: *** IR for main
CHECK-NEXT: function u0:10(i32, i64) -> i32 system_v {
CHECK-NEXT:     gv0 = symbol colocated userextname0
CHECK-NEXT:     sig0 = (i64, i64) system_v
CHECK-NEXT:     fn0 = colocated u0:9 sig0
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i32, v1: i64):
CHECK-NEXT:     v2 = iconst.i64 3
CHECK-NEXT:     v3 = global_value.i64 gv0
CHECK-NEXT:     store v2, v3  ; v2 = 3
CHECK-NEXT:     v4 = iconst.i64 1
CHECK-NEXT:     v5 = global_value.i64 gv0
CHECK-NEXT:     call fn0(v4, v5)  ; v4 = 1
CHECK-NEXT:     v6 = iconst.i32 0
CHECK-NEXT:     return v6  ; v6 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for main seems OK

}

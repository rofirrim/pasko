{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main;
var
  z : integer;

function increment(x: integer) : integer;
begin
    increment := x + 1;
end;

begin
  z := increment(41);
end.

{

CHECK: *** IR for function 'increment'
CHECK-NEXT: function u0:21(i64) -> i64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.i64 v2
CHECK-NEXT:     v4 = iconst.i64 1
CHECK-NEXT:     v5 = iadd v3, v4  ; v4 = 1
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     store v5, v6
CHECK-NEXT:     v7 = stack_addr.i64 ss0
CHECK-NEXT:     v8 = load.i64 v7
CHECK-NEXT:     return v8
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'increment' seems OK

CHECK: *** IR for main
CHECK-NEXT: function u0:22(i32, i64) -> i32 system_v {
CHECK-NEXT:     gv0 = symbol colocated userextname1
CHECK-NEXT:     sig0 = (i64) -> i64 system_v
CHECK-NEXT:     fn0 = colocated u0:21 sig0
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i32, v1: i64):
CHECK-NEXT:     v2 = iconst.i64 41
CHECK-NEXT:     v3 = call fn0(v2)  ; v2 = 41
CHECK-NEXT:     v4 = global_value.i64 gv0
CHECK-NEXT:     store v3, v4
CHECK-NEXT:     v5 = iconst.i32 0
CHECK-NEXT:     return v5  ; v5 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for main seems OK

}

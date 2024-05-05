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

CHECK: *** IR for main
CHECK-NEXT: function u0:20(i32, i64) -> i32 system_v {
CHECK-NEXT:     gv0 = symbol colocated userextname1
CHECK-NEXT:     sig0 = (i64) -> i64 system_v
CHECK-NEXT:     fn0 = colocated u0:19 sig0
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

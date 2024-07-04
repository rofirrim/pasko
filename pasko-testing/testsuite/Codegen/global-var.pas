{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main;
var
  a: integer;

begin
  a := 2;
  a := a + 1;
end.

{
CHECK: *** IR for main
CHECK-NEXT: function u0:21(i32, i64) -> i32 system_v {
CHECK-NEXT:     gv0 = symbol colocated userextname0
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i32, v1: i64):
CHECK-NEXT:     v2 = iconst.i64 2
CHECK-NEXT:     v3 = global_value.i64 gv0
CHECK-NEXT:     store v2, v3  ; v2 = 2
CHECK-NEXT:     v4 = global_value.i64 gv0
CHECK-NEXT:     v5 = load.i64 v4
CHECK-NEXT:     v6 = iconst.i64 1
CHECK-NEXT:     v7 = iadd v5, v6  ; v6 = 1
CHECK-NEXT:     v8 = global_value.i64 gv0
CHECK-NEXT:     store v7, v8
CHECK-NEXT:     v9 = iconst.i32 0
CHECK-NEXT:     return v9  ; v9 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for main seems OK
}

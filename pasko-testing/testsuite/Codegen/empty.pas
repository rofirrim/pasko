{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main;

begin
end.

{
CHECK:      *** IR for main
CHECK-NEXT: function u0:19(i32, i64) -> i32 system_v {
CHECK-NEXT: block0(v0: i32, v1: i64):
CHECK-NEXT:     v2 = iconst.i32 0
CHECK-NEXT:     return v2  ; v2 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for main seems OK
}

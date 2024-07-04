{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program test;

var
  c: char;

begin
  c := 'a';
end.

{

CHECK: *** IR for main
CHECK-NEXT: function u0:21(i32, i64) -> i32 system_v {
CHECK-NEXT:     gv0 = symbol colocated userextname0
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i32, v1: i64):
CHECK-NEXT:     v2 = iconst.i32 97
CHECK-NEXT:     v3 = global_value.i64 gv0
CHECK-NEXT:     store v2, v3  ; v2 = 97
CHECK-NEXT:     v4 = iconst.i32 0
CHECK-NEXT:     return v4  ; v4 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for main seems OK

}

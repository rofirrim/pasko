{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main;

var
  a: array[1..10] of integer;

begin
  a[1] := 3;
end.

{

CHECK:      *** IR for main
CHECK-NEXT: function u0:21(i32, i64) -> i32 system_v {
CHECK-NEXT:     gv0 = symbol colocated userextname0
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i32, v1: i64):
CHECK-NEXT:     v2 = iconst.i64 3
CHECK-NEXT:     v3 = global_value.i64 gv0
CHECK-NEXT:     v4 = iconst.i64 1
CHECK-NEXT:     v5 = iconst.i64 1
CHECK-NEXT:     v6 = isub v4, v5  ; v4 = 1, v5 = 1
CHECK-NEXT:     v7 = iconst.i64 8
CHECK-NEXT:     v8 = imul v6, v7  ; v7 = 8
CHECK-NEXT:     v9 = iadd v3, v8
CHECK-NEXT:     store v2, v9  ; v2 = 3
CHECK-NEXT:     v10 = iconst.i32 0
CHECK-NEXT:     return v10  ; v10 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for main seems OK

}


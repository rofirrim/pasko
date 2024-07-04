{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program test;

var
  a: ^integer;
  b: ^integer;

begin
  new(a);
  b := a;

  a^ := 42;

  dispose(a);
end.

{

CHECK: *** IR for main
CHECK-NEXT: function u0:21(i32, i64) -> i32 system_v {
CHECK-NEXT:     gv0 = symbol colocated userextname0
CHECK-NEXT:     gv1 = symbol colocated userextname2
CHECK-NEXT:     sig0 = (i64, i64) system_v
CHECK-NEXT:     sig1 = (i64) system_v
CHECK-NEXT:     fn0 = u0:19 sig0
CHECK-NEXT:     fn1 = u0:20 sig1
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i32, v1: i64):
CHECK-NEXT:     v2 = global_value.i64 gv0
CHECK-NEXT:     v3 = iconst.i64 8
CHECK-NEXT:     call fn0(v2, v3)  ; v3 = 8
CHECK-NEXT:     v4 = global_value.i64 gv0
CHECK-NEXT:     v5 = load.i64 v4
CHECK-NEXT:     v6 = global_value.i64 gv1
CHECK-NEXT:     store v5, v6
CHECK-NEXT:     v7 = iconst.i64 42
CHECK-NEXT:     v8 = global_value.i64 gv0
CHECK-NEXT:     v9 = load.i64 v8
CHECK-NEXT:     store v7, v9  ; v7 = 42
CHECK-NEXT:     v10 = global_value.i64 gv0
CHECK-NEXT:     call fn1(v10)
CHECK-NEXT:     v11 = iconst.i32 0
CHECK-NEXT:     return v11  ; v11 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for main seems OK

}

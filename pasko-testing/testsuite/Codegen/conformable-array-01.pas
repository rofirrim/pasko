{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main;

var
  a: array[1..10] of integer;

procedure init(var x : array[l .. u: integer] of integer);
begin
  x[l] := l;
  x[u] := u;
end;

begin
  init(a);
end.

{

CHECK: *** IR for 'init'
CHECK-NEXT: function u0:0(i64, i64, i64) system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; [indirect] x
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64, v1: i64, v2: i64):
CHECK-NEXT:     v3 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v3
CHECK-NEXT:     v4 = stack_addr.i64 ss0
CHECK-NEXT:     v5 = load.i64 v4
CHECK-NEXT:     v6 = iconst.i64 8
CHECK-NEXT:     v7 = isub v1, v1
CHECK-NEXT:     v8 = imul v7, v6  ; v6 = 8
CHECK-NEXT:     v9 = iadd v5, v8
CHECK-NEXT:     store v1, v9
CHECK-NEXT:     v10 = stack_addr.i64 ss0
CHECK-NEXT:     v11 = load.i64 v10
CHECK-NEXT:     v12 = iconst.i64 8
CHECK-NEXT:     v13 = isub v2, v1
CHECK-NEXT:     v14 = imul v13, v12  ; v12 = 8
CHECK-NEXT:     v15 = iadd v11, v14
CHECK-NEXT:     store v2, v15
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'init' seems OK

}

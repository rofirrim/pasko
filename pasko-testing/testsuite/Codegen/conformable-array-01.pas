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
CHECK-NEXT: block0(v0: i64, v1: i64, v2: i64):
CHECK-NEXT:     v3 = iconst.i64 8
CHECK-NEXT:     v4 = isub v1, v1
CHECK-NEXT:     v5 = imul v4, v3  ; v3 = 8
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v6 = iadd v0, v5
CHECK-NEXT:     store v1, v6
CHECK-NEXT:     v7 = iconst.i64 8
CHECK-NEXT:     v8 = isub v2, v1
CHECK-NEXT:     v9 = imul v8, v7  ; v7 = 8
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v10 = iadd v0, v9
CHECK-NEXT:     store v2, v10
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'init' seems OK

}

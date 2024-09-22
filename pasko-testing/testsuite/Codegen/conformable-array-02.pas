{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main;

function lbound(var x : array[l .. u: integer] of integer) : integer;
begin
  lbound := l;
end;

function ubound(var x : array[l .. u: integer] of integer) : integer;
begin
  ubound := u;
end;

begin
end.

{

CHECK: *** IR for 'lbound'
CHECK-NEXT: function u0:0(i64, i64, i64) -> i64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; lbound
CHECK-NEXT:     ss1 = explicit_slot 8 ; [indirect] x
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64, v1: i64, v2: i64):
CHECK-NEXT:     v3 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v3
CHECK-NEXT:     v4 = stack_addr.i64 ss0
CHECK-NEXT:     store v1, v4
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     v6 = load.i64 v5
CHECK-NEXT:     return v6
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'lbound' seems OK

CHECK: *** IR for 'ubound'
CHECK-NEXT: function u0:1(i64, i64, i64) -> i64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; ubound
CHECK-NEXT:     ss1 = explicit_slot 8 ; [indirect] x
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64, v1: i64, v2: i64):
CHECK-NEXT:     v3 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v3
CHECK-NEXT:     v4 = stack_addr.i64 ss0
CHECK-NEXT:     store v2, v4
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     v6 = load.i64 v5
CHECK-NEXT:     return v6
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'ubound' seems OK

}


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
CHECK-NEXT: block0(v0: i64, v1: i64, v2: i64):
CHECK-NEXT:     ! v1 → lbound 
CHECK-NEXT:     return v1
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'lbound' seems OK

CHECK: *** IR for 'ubound'
CHECK-NEXT: function u0:1(i64, i64, i64) -> i64 system_v {
CHECK-NEXT: block0(v0: i64, v1: i64, v2: i64):
CHECK-NEXT:     ! v2 → ubound 
CHECK-NEXT:     return v2
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'ubound' seems OK

}


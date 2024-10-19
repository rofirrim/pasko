{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main;
procedure p;
var
  a: integer;
begin
  a := 2;
  a := a + 1;
end;

begin
end.

{

CHECK: *** IR for 'p'
CHECK-NEXT: function u0:0() system_v {
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = iconst.i64 2
CHECK-NEXT:     ! a ← v0 
CHECK-NEXT:     v1 = iconst.i64 1
CHECK-NEXT:     ! v0 → a 
CHECK-NEXT:     v2 = iadd v0, v1  ; v0 = 2, v1 = 1
CHECK-NEXT:     ! a ← v2 
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'p' seems OK

}

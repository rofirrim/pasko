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
CHECK: *** IR for procedure 'p'
CHECK-NEXT: function u0:21() system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-EMPTY:
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = iconst.i64 2
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v1  ; v0 = 2
CHECK-NEXT:     v2 = stack_addr.i64 ss0
CHECK-NEXT:     v3 = load.i64 v2
CHECK-NEXT:     v4 = iconst.i64 1
CHECK-NEXT:     v5 = iadd v3, v4  ; v4 = 1
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     store v5, v6
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for procedure 'p' seems OK
}

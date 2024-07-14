{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program test;

type
  t = set of integer;

procedure foo_val(a: t);
begin
  a := [51];
end;

procedure foo_ref(var a: t);
begin
  a := [42];
end;

begin
end.

{

CHECK: *** IR for procedure 'foo_val'
CHECK-NEXT: function u0:38(i64) system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-NEXT:     sig0 = (i64, i64) -> i64 system_v
CHECK-NEXT:     sig1 = (i64) system_v
CHECK-NEXT:     fn0 = u0:9 sig0
CHECK-NEXT:     fn1 = u0:10 sig1
CHECK-EMPTY: 
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = iconst.i64 51
CHECK-NEXT:     v3 = stack_addr.i64 ss1
CHECK-NEXT:     store v2, v3  ; v2 = 51
CHECK-NEXT:     v4 = iconst.i64 1
CHECK-NEXT:     v5 = call fn0(v4, v3)  ; v4 = 1
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.i64 v6
CHECK-NEXT:     call fn1(v7)
CHECK-NEXT:     store v5, v6
CHECK-NEXT:     v8 = stack_addr.i64 ss0
CHECK-NEXT:     v9 = load.i64 v8
CHECK-NEXT:     call fn1(v9)
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for procedure 'foo_val' seems OK

CHECK: *** IR for procedure 'foo_ref'
CHECK-NEXT: function u0:39(i64) system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-NEXT:     sig0 = (i64, i64) -> i64 system_v
CHECK-NEXT:     sig1 = (i64) system_v
CHECK-NEXT:     fn0 = u0:9 sig0
CHECK-NEXT:     fn1 = u0:10 sig1
CHECK-EMPTY: 
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = iconst.i64 42
CHECK-NEXT:     v3 = stack_addr.i64 ss1
CHECK-NEXT:     store v2, v3  ; v2 = 42
CHECK-NEXT:     v4 = iconst.i64 1
CHECK-NEXT:     v5 = call fn0(v4, v3)  ; v4 = 1
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.i64 v6
CHECK-NEXT:     v8 = load.i64 v7
CHECK-NEXT:     call fn1(v8)
CHECK-NEXT:     store v5, v7
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for procedure 'foo_ref' seems OK

}

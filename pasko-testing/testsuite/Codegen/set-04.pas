{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}
program main(output);

procedure foo;
var
  a: set of integer;
  i: integer;

begin
  a := [1..5];
end;

begin
  foo;
end.

{

CHECK: *** IR for 'foo'
CHECK-NEXT: function u0:0() system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; a
CHECK-NEXT:     ss1 = explicit_slot 8 ; [set-constructor-tmp-set]
CHECK-NEXT:     ss2 = explicit_slot 8 ; [set-constructor-ith-element]
CHECK-NEXT:     sig0 = (i64, i64) -> i64 system_v
CHECK-NEXT:     sig1 = (i64, i64) -> i64 system_v
CHECK-NEXT:     sig2 = (i64) system_v
CHECK-NEXT:     fn0 = u0:1 sig0 ; __pasko_set_new
CHECK-NEXT:     fn1 = u0:2 sig1 ; __pasko_set_union
CHECK-NEXT:     fn2 = u0:3 sig2 ; __pasko_set_dispose
CHECK-EMPTY:
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = stack_addr.i64 ss0
CHECK-NEXT:     v1 = iconst.i64 0
CHECK-NEXT:     store v1, v0  ; v1 = 0
CHECK-NEXT:     v2 = iconst.i64 1
CHECK-NEXT:     v3 = iconst.i64 5
CHECK-NEXT:     v4 = iconst.i64 0
CHECK-NEXT:     v5 = call fn0(v4, v4)  ; v4 = 0, v4 = 0
CHECK-NEXT:     v6 = stack_addr.i64 ss1
CHECK-NEXT:     stack_store v5, ss1
CHECK-NEXT:     v7 = stack_addr.i64 ss2
CHECK-NEXT:     v8 = iconst.i64 1
CHECK-NEXT:     v9 = icmp sgt v2, v3  ; v2 = 1, v3 = 5
CHECK-NEXT:     brif v9, block2, block1
CHECK-EMPTY:
CHECK-NEXT: block1:
CHECK-NEXT:     jump block3(v2)  ; v2 = 1
CHECK-EMPTY:
CHECK-NEXT: block3(v10: i64):
CHECK-NEXT:     v11 = isub v10, v2  ; v2 = 1
CHECK-NEXT:     v12 = iadd.i64 v2, v11  ; v2 = 1
CHECK-NEXT:     stack_store v12, ss2
CHECK-NEXT:     v13 = call fn0(v8, v7)  ; v8 = 1
CHECK-NEXT:     v14 = load.i64 v6
CHECK-NEXT:     v15 = call fn1(v14, v13)
CHECK-NEXT:     call fn2(v13)
CHECK-NEXT:     call fn2(v14)
CHECK-NEXT:     store v15, v6
CHECK-NEXT:     v16 = icmp eq v10, v3  ; v3 = 5
CHECK-NEXT:     brif v16, block2, block4
CHECK-EMPTY:
CHECK-NEXT: block4:
CHECK-NEXT:     v17 = iadd_imm.i64 v10, 1
CHECK-NEXT:     jump block3(v17)
CHECK-EMPTY:
CHECK-NEXT: block2:
CHECK-NEXT:     v18 = load.i64 v6
CHECK-NEXT:     v19 = stack_addr.i64 ss0
CHECK-NEXT:     v20 = load.i64 v19
CHECK-NEXT:     call fn2(v20)
CHECK-NEXT:     store v18, v19
CHECK-NEXT:     v21 = stack_addr.i64 ss0
CHECK-NEXT:     v22 = load.i64 v21
CHECK-NEXT:     call fn2(v22)
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'foo' seems OK

}

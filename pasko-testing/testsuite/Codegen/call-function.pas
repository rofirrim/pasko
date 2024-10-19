{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main;
var
  z : integer;

function increment(x: integer) : integer;
begin
    increment := x + 1;
end;

begin
  z := increment(41);
end.

{

CHECK: *** IR for 'increment'
CHECK-NEXT: function u0:0(i64) -> i64 system_v {
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = iconst.i64 1
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v2 = iadd v0, v1  ; v1 = 1
CHECK-NEXT:     ! increment ← v2 
CHECK-NEXT:     ! v2 → increment 
CHECK-NEXT:     return v2
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'increment' seems OK

CHECK: *** IR for 'main'
CHECK-NEXT: function u0:1(i32, i64) -> i32 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; [null-ended-array: program-parameter-names]
CHECK-NEXT:     ss1 = explicit_slot 8 ; [null-ended-array: global-files]
CHECK-NEXT:     gv0 = symbol colocated userextname4 ; [input-textfile]
CHECK-NEXT:     gv1 = symbol colocated userextname5 ; [output-textfile]
CHECK-NEXT:     gv2 = symbol colocated userextname7 ; z
CHECK-NEXT:     sig0 = (i32, i64, i32, i64, i32, i64) system_v
CHECK-NEXT:     sig1 = (i32, i64) system_v
CHECK-NEXT:     sig2 = () -> i64 system_v
CHECK-NEXT:     sig3 = () -> i64 system_v
CHECK-NEXT:     sig4 = (i64) -> i64 system_v
CHECK-NEXT:     fn0 = u0:2 sig0 ; __pasko_init
CHECK-NEXT:     fn1 = u0:3 sig1 ; __pasko_finish
CHECK-NEXT:     fn2 = u0:4 sig2 ; __pasko_get_input
CHECK-NEXT:     fn3 = u0:5 sig3 ; __pasko_get_output
CHECK-NEXT:     fn4 = colocated u0:0 sig4 ; increment
CHECK-EMPTY: 
CHECK-NEXT: block0(v0: i32, v1: i64):
CHECK-NEXT:     v2 = iconst.i32 0
CHECK-NEXT:     v3 = iconst.i64 0
CHECK-NEXT:     stack_store v3, ss0  ; v3 = 0
CHECK-NEXT:     v4 = stack_addr.i64 ss0
CHECK-NEXT:     v5 = iconst.i64 0
CHECK-NEXT:     stack_store v5, ss1  ; v5 = 0
CHECK-NEXT:     v6 = stack_addr.i64 ss1
CHECK-NEXT:     v7 = iconst.i32 0
CHECK-NEXT:     call fn0(v0, v1, v2, v4, v7, v6)  ; v2 = 0, v7 = 0
CHECK-NEXT:     v8 = call fn2()
CHECK-NEXT:     v9 = global_value.i64 gv0
CHECK-NEXT:     store v8, v9
CHECK-NEXT:     v10 = call fn3()
CHECK-NEXT:     v11 = global_value.i64 gv1
CHECK-NEXT:     store v10, v11
CHECK-NEXT:     v12 = iconst.i64 41
CHECK-NEXT:     v13 = call fn4(v12)  ; v12 = 41
CHECK-NEXT:     v14 = global_value.i64 gv2
CHECK-NEXT:     store v13, v14
CHECK-NEXT:     call fn1(v7, v6)  ; v7 = 0
CHECK-NEXT:     v15 = iconst.i32 0
CHECK-NEXT:     return v15  ; v15 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'main' seems OK

}

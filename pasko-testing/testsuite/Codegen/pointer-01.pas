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

CHECK: *** IR for 'main'
CHECK-NEXT: function u0:0(i32, i64) -> i32 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; [null-ended-array: program-parameter-names]
CHECK-NEXT:     ss1 = explicit_slot 8 ; [null-ended-array: global-files]
CHECK-NEXT:     gv0 = symbol colocated userextname4 ; [input-textfile]
CHECK-NEXT:     gv1 = symbol colocated userextname5 ; [output-textfile]
CHECK-NEXT:     gv2 = symbol colocated userextname6 ; a
CHECK-NEXT:     gv3 = symbol colocated userextname8 ; b
CHECK-NEXT:     sig0 = (i32, i64, i32, i64, i32, i64) system_v
CHECK-NEXT:     sig1 = (i32, i64) system_v
CHECK-NEXT:     sig2 = () -> i64 system_v
CHECK-NEXT:     sig3 = () -> i64 system_v
CHECK-NEXT:     sig4 = (i64) -> i64 system_v
CHECK-NEXT:     sig5 = (i64) system_v
CHECK-NEXT:     fn0 = u0:1 sig0 ; __pasko_init
CHECK-NEXT:     fn1 = u0:2 sig1 ; __pasko_finish
CHECK-NEXT:     fn2 = u0:3 sig2 ; __pasko_get_input
CHECK-NEXT:     fn3 = u0:4 sig3 ; __pasko_get_output
CHECK-NEXT:     fn4 = u0:5 sig4 ; __pasko_pointer_new
CHECK-NEXT:     fn5 = u0:6 sig5 ; __pasko_pointer_dispose
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
CHECK-NEXT:     v12 = global_value.i64 gv2
CHECK-NEXT:     v13 = iconst.i64 8
CHECK-NEXT:     v14 = call fn4(v13)  ; v13 = 8
CHECK-NEXT:     store v14, v12
CHECK-NEXT:     v15 = global_value.i64 gv2
CHECK-NEXT:     v16 = load.i64 v15
CHECK-NEXT:     v17 = global_value.i64 gv3
CHECK-NEXT:     store v16, v17
CHECK-NEXT:     v18 = iconst.i64 42
CHECK-NEXT:     v19 = global_value.i64 gv2
CHECK-NEXT:     v20 = load.i64 v19
CHECK-NEXT:     store v18, v20  ; v18 = 42
CHECK-NEXT:     v21 = global_value.i64 gv2
CHECK-NEXT:     v22 = load.i64 v21
CHECK-NEXT:     call fn5(v22)
CHECK-NEXT:     call fn1(v7, v6)  ; v7 = 0
CHECK-NEXT:     v23 = iconst.i32 0
CHECK-NEXT:     return v23  ; v23 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'main' seems OK

}

{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program test;

type
  my_list = record
    x: integer;
    next: ^my_list;
  end;

var
  list: ^my_list;

begin
  new(list);
  list^.x := 1;

  new(list^.next);
  list^.next^.x := 2;
  list^.next^.next := nil;

  dispose(list^.next);
  dispose(list);
end.

{

CHECK: *** IR for 'main'
CHECK-NEXT: function u0:0(i32, i64) -> i32 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-NEXT:     gv0 = symbol colocated userextname4
CHECK-NEXT:     gv1 = symbol colocated userextname5
CHECK-NEXT:     gv2 = symbol colocated userextname6
CHECK-NEXT:     sig0 = (i32, i64, i32, i64, i32, i64) system_v
CHECK-NEXT:     sig1 = (i32, i64) system_v
CHECK-NEXT:     sig2 = () -> i64 system_v
CHECK-NEXT:     sig3 = () -> i64 system_v
CHECK-NEXT:     sig4 = (i64, i64) system_v
CHECK-NEXT:     sig5 = (i64) system_v
CHECK-NEXT:     fn0 = u0:1 sig0
CHECK-NEXT:     fn1 = u0:2 sig1
CHECK-NEXT:     fn2 = u0:3 sig2
CHECK-NEXT:     fn3 = u0:4 sig3
CHECK-NEXT:     fn4 = u0:5 sig4
CHECK-NEXT:     fn5 = u0:6 sig5
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
CHECK-NEXT:     v13 = iconst.i64 16
CHECK-NEXT:     call fn4(v12, v13)  ; v13 = 16
CHECK-NEXT:     v14 = iconst.i64 1
CHECK-NEXT:     v15 = global_value.i64 gv2
CHECK-NEXT:     v16 = load.i64 v15
CHECK-NEXT:     store v14, v16  ; v14 = 1
CHECK-NEXT:     v17 = global_value.i64 gv2
CHECK-NEXT:     v18 = load.i64 v17
CHECK-NEXT:     v19 = iconst.i64 8
CHECK-NEXT:     v20 = iadd v18, v19  ; v19 = 8
CHECK-NEXT:     v21 = iconst.i64 16
CHECK-NEXT:     call fn4(v20, v21)  ; v21 = 16
CHECK-NEXT:     v22 = iconst.i64 2
CHECK-NEXT:     v23 = global_value.i64 gv2
CHECK-NEXT:     v24 = load.i64 v23
CHECK-NEXT:     v25 = iconst.i64 8
CHECK-NEXT:     v26 = iadd v24, v25  ; v25 = 8
CHECK-NEXT:     v27 = load.i64 v26
CHECK-NEXT:     store v22, v27  ; v22 = 2
CHECK-NEXT:     v28 = iconst.i64 0
CHECK-NEXT:     v29 = global_value.i64 gv2
CHECK-NEXT:     v30 = load.i64 v29
CHECK-NEXT:     v31 = iconst.i64 8
CHECK-NEXT:     v32 = iadd v30, v31  ; v31 = 8
CHECK-NEXT:     v33 = load.i64 v32
CHECK-NEXT:     v34 = iconst.i64 8
CHECK-NEXT:     v35 = iadd v33, v34  ; v34 = 8
CHECK-NEXT:     store v28, v35  ; v28 = 0
CHECK-NEXT:     v36 = global_value.i64 gv2
CHECK-NEXT:     v37 = load.i64 v36
CHECK-NEXT:     v38 = iconst.i64 8
CHECK-NEXT:     v39 = iadd v37, v38  ; v38 = 8
CHECK-NEXT:     call fn5(v39)
CHECK-NEXT:     v40 = global_value.i64 gv2
CHECK-NEXT:     call fn5(v40)
CHECK-NEXT:     call fn1(v7, v6)  ; v7 = 0
CHECK-NEXT:     v41 = iconst.i32 0
CHECK-NEXT:     return v41  ; v41 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'main' seems OK

}

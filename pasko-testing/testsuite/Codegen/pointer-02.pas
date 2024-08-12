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

CHECK: *** IR for main
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
CHECK-NEXT:     v17 = iconst.i64 0
CHECK-NEXT:     v18 = iadd v16, v17  ; v17 = 0
CHECK-NEXT:     store v14, v18  ; v14 = 1
CHECK-NEXT:     v19 = global_value.i64 gv2
CHECK-NEXT:     v20 = load.i64 v19
CHECK-NEXT:     v21 = iconst.i64 8
CHECK-NEXT:     v22 = iadd v20, v21  ; v21 = 8
CHECK-NEXT:     v23 = iconst.i64 16
CHECK-NEXT:     call fn4(v22, v23)  ; v23 = 16
CHECK-NEXT:     v24 = iconst.i64 2
CHECK-NEXT:     v25 = global_value.i64 gv2
CHECK-NEXT:     v26 = load.i64 v25
CHECK-NEXT:     v27 = iconst.i64 8
CHECK-NEXT:     v28 = iadd v26, v27  ; v27 = 8
CHECK-NEXT:     v29 = load.i64 v28
CHECK-NEXT:     v30 = iconst.i64 0
CHECK-NEXT:     v31 = iadd v29, v30  ; v30 = 0
CHECK-NEXT:     store v24, v31  ; v24 = 2
CHECK-NEXT:     v32 = iconst.i64 0
CHECK-NEXT:     v33 = global_value.i64 gv2
CHECK-NEXT:     v34 = load.i64 v33
CHECK-NEXT:     v35 = iconst.i64 8
CHECK-NEXT:     v36 = iadd v34, v35  ; v35 = 8
CHECK-NEXT:     v37 = load.i64 v36
CHECK-NEXT:     v38 = iconst.i64 8
CHECK-NEXT:     v39 = iadd v37, v38  ; v38 = 8
CHECK-NEXT:     store v32, v39  ; v32 = 0
CHECK-NEXT:     v40 = global_value.i64 gv2
CHECK-NEXT:     v41 = load.i64 v40
CHECK-NEXT:     v42 = iconst.i64 8
CHECK-NEXT:     v43 = iadd v41, v42  ; v42 = 8
CHECK-NEXT:     call fn5(v43)
CHECK-NEXT:     v44 = global_value.i64 gv2
CHECK-NEXT:     call fn5(v44)
CHECK-NEXT:     call fn1(v7, v6)  ; v7 = 0
CHECK-NEXT:     v45 = iconst.i32 0
CHECK-NEXT:     return v45  ; v45 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for main seems OK

}

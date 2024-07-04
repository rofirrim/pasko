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
CHECK-NEXT: function u0:21(i32, i64) -> i32 system_v {
CHECK-NEXT:     gv0 = symbol colocated userextname0
CHECK-NEXT:     sig0 = (i64, i64) system_v
CHECK-NEXT:     sig1 = (i64) system_v
CHECK-NEXT:     fn0 = u0:19 sig0
CHECK-NEXT:     fn1 = u0:20 sig1
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i32, v1: i64):
CHECK-NEXT:     v2 = global_value.i64 gv0
CHECK-NEXT:     v3 = iconst.i64 16
CHECK-NEXT:     call fn0(v2, v3)  ; v3 = 16
CHECK-NEXT:     v4 = iconst.i64 1
CHECK-NEXT:     v5 = global_value.i64 gv0
CHECK-NEXT:     v6 = load.i64 v5
CHECK-NEXT:     v7 = iconst.i64 0
CHECK-NEXT:     v8 = iadd v6, v7  ; v7 = 0
CHECK-NEXT:     store v4, v8  ; v4 = 1
CHECK-NEXT:     v9 = global_value.i64 gv0
CHECK-NEXT:     v10 = load.i64 v9
CHECK-NEXT:     v11 = iconst.i64 8
CHECK-NEXT:     v12 = iadd v10, v11  ; v11 = 8
CHECK-NEXT:     v13 = iconst.i64 16
CHECK-NEXT:     call fn0(v12, v13)  ; v13 = 16
CHECK-NEXT:     v14 = iconst.i64 2
CHECK-NEXT:     v15 = global_value.i64 gv0
CHECK-NEXT:     v16 = load.i64 v15
CHECK-NEXT:     v17 = iconst.i64 8
CHECK-NEXT:     v18 = iadd v16, v17  ; v17 = 8
CHECK-NEXT:     v19 = load.i64 v18
CHECK-NEXT:     v20 = iconst.i64 0
CHECK-NEXT:     v21 = iadd v19, v20  ; v20 = 0
CHECK-NEXT:     store v14, v21  ; v14 = 2
CHECK-NEXT:     v22 = iconst.i64 0
CHECK-NEXT:     v23 = global_value.i64 gv0
CHECK-NEXT:     v24 = load.i64 v23
CHECK-NEXT:     v25 = iconst.i64 8
CHECK-NEXT:     v26 = iadd v24, v25  ; v25 = 8
CHECK-NEXT:     v27 = load.i64 v26
CHECK-NEXT:     v28 = iconst.i64 8
CHECK-NEXT:     v29 = iadd v27, v28  ; v28 = 8
CHECK-NEXT:     store v22, v29  ; v22 = 0
CHECK-NEXT:     v30 = global_value.i64 gv0
CHECK-NEXT:     v31 = load.i64 v30
CHECK-NEXT:     v32 = iconst.i64 8
CHECK-NEXT:     v33 = iadd v31, v32  ; v32 = 8
CHECK-NEXT:     call fn1(v33)
CHECK-NEXT:     v34 = global_value.i64 gv0
CHECK-NEXT:     call fn1(v34)
CHECK-NEXT:     v35 = iconst.i32 0
CHECK-NEXT:     return v35  ; v35 = 0
CHECK-NEXT: }

}

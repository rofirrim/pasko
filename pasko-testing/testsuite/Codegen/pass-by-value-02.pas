{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main;
type
   myset = set of integer;
   myrecord2 = record
      a: integer;
      b: myset;
   end;
   myrecord = record
      a: integer;
      b: myrecord2;
   end;

procedure sink_2(a: myrecord);
var
  x : myrecord;
begin
  x.b.b := [1, 2, 3];
end;

procedure source_2;
var
  x : myrecord;
begin
  sink_2(x);
end;

begin
end.

{

CHECK: *** IR for 'sink_2'
CHECK-NEXT: function u0:0(i64) system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; [indirect] a
CHECK-NEXT:     ss1 = explicit_slot 24 ; x
CHECK-NEXT:     ss2 = explicit_slot 24 ; [set-constructor]
CHECK-NEXT:     sig0 = (i64, i64) -> i64 system_v
CHECK-NEXT:     sig1 = (i64) system_v
CHECK-NEXT:     fn0 = u0:1 sig0 ; __pasko_set_new
CHECK-NEXT:     fn1 = u0:2 sig1 ; __pasko_set_dispose
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = iconst.i64 8
CHECK-NEXT:     v4 = iadd v2, v3  ; v3 = 8
CHECK-NEXT:     v5 = iconst.i64 8
CHECK-NEXT:     v6 = iadd v4, v5  ; v5 = 8
CHECK-NEXT:     v7 = iconst.i64 0
CHECK-NEXT:     store v7, v6  ; v7 = 0
CHECK-NEXT:     v8 = iconst.i64 1
CHECK-NEXT:     v9 = iconst.i64 2
CHECK-NEXT:     v10 = iconst.i64 3
CHECK-NEXT:     v11 = stack_addr.i64 ss2
CHECK-NEXT:     store v8, v11  ; v8 = 1
CHECK-NEXT:     store v9, v11+8  ; v9 = 2
CHECK-NEXT:     store v10, v11+16  ; v10 = 3
CHECK-NEXT:     v12 = iconst.i64 3
CHECK-NEXT:     v13 = call fn0(v12, v11)  ; v12 = 3
CHECK-NEXT:     v14 = stack_addr.i64 ss1
CHECK-NEXT:     v15 = iconst.i64 8
CHECK-NEXT:     v16 = iadd v14, v15  ; v15 = 8
CHECK-NEXT:     v17 = iconst.i64 8
CHECK-NEXT:     v18 = iadd v16, v17  ; v17 = 8
CHECK-NEXT:     v19 = load.i64 v18
CHECK-NEXT:     call fn1(v19)
CHECK-NEXT:     store v13, v18
CHECK-NEXT:     v20 = stack_addr.i64 ss0
CHECK-NEXT:     v21 = load.i64 v20
CHECK-NEXT:     v22 = iconst.i64 8
CHECK-NEXT:     v23 = iadd v21, v22  ; v22 = 8
CHECK-NEXT:     v24 = iconst.i64 8
CHECK-NEXT:     v25 = iadd v23, v24  ; v24 = 8
CHECK-NEXT:     v26 = load.i64 v25
CHECK-NEXT:     call fn1(v26)
CHECK-NEXT:     v27 = stack_addr.i64 ss1
CHECK-NEXT:     v28 = iconst.i64 8
CHECK-NEXT:     v29 = iadd v27, v28  ; v28 = 8
CHECK-NEXT:     v30 = iconst.i64 8
CHECK-NEXT:     v31 = iadd v29, v30  ; v30 = 8
CHECK-NEXT:     v32 = load.i64 v31
CHECK-NEXT:     call fn1(v32)
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'sink_2' seems OK

CHECK: *** IR for 'source_2'
CHECK-NEXT: function u0:3() system_v {
CHECK-NEXT:     ss0 = explicit_slot 24 ; x
CHECK-NEXT:     ss1 = explicit_slot 24 ; [copy-in]
CHECK-NEXT:     sig0 = (i64) -> i64 system_v
CHECK-NEXT:     sig1 = (i64) system_v
CHECK-NEXT:     sig2 = (i64) system_v
CHECK-NEXT:     fn0 = u0:4 sig0 ; __pasko_set_copy
CHECK-NEXT:     fn1 = colocated u0:0 sig1 ; sink_2
CHECK-NEXT:     fn2 = u0:2 sig2 ; __pasko_set_dispose
CHECK-EMPTY:
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = stack_addr.i64 ss0
CHECK-NEXT:     v1 = iconst.i64 8
CHECK-NEXT:     v2 = iadd v0, v1  ; v1 = 8
CHECK-NEXT:     v3 = iconst.i64 8
CHECK-NEXT:     v4 = iadd v2, v3  ; v3 = 8
CHECK-NEXT:     v5 = iconst.i64 0
CHECK-NEXT:     store v5, v4  ; v5 = 0
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = stack_addr.i64 ss1
CHECK-NEXT:     v8 = load.i64 v6
CHECK-NEXT:     store v8, v7
CHECK-NEXT:     v9 = iconst.i64 8
CHECK-NEXT:     v10 = iadd v7, v9  ; v9 = 8
CHECK-NEXT:     v11 = iconst.i64 8
CHECK-NEXT:     v12 = iadd v6, v11  ; v11 = 8
CHECK-NEXT:     v13 = load.i64 v12
CHECK-NEXT:     store v13, v10
CHECK-NEXT:     v14 = iconst.i64 8
CHECK-NEXT:     v15 = iadd v10, v14  ; v14 = 8
CHECK-NEXT:     v16 = iconst.i64 8
CHECK-NEXT:     v17 = iadd v12, v16  ; v16 = 8
CHECK-NEXT:     v18 = load.i64 v17
CHECK-NEXT:     v19 = call fn0(v18)
CHECK-NEXT:     store v19, v15
CHECK-NEXT:     call fn1(v7)
CHECK-NEXT:     v20 = stack_addr.i64 ss0
CHECK-NEXT:     v21 = iconst.i64 8
CHECK-NEXT:     v22 = iadd v20, v21  ; v21 = 8
CHECK-NEXT:     v23 = iconst.i64 8
CHECK-NEXT:     v24 = iadd v22, v23  ; v23 = 8
CHECK-NEXT:     v25 = load.i64 v24
CHECK-NEXT:     call fn2(v25)
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'source_2' seems OK

}

{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main(output);

var
  a: array[1..10, 2..30] of integer;

procedure foo(var x: array[u0..l0: integer; u1..l1:integer] of integer);
begin
  x[u0, l0] := 0;
  x[u1, l1] := 0;
end;

procedure foo2(x: array[u0..l0: integer; u1..l1:integer] of integer);
begin
  x[u0, l0] := 0;
  x[u1, l1] := 0;
end;

procedure mymain1;
begin
  foo(a);
end;

procedure mymain2;
begin
  foo2(a);
end;

begin
  mymain1;
  mymain2;
end.

{

CHECK: *** IR for 'foo'
CHECK-NEXT: function u0:0(i64, i64, i64, i64, i64) system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; [indirect] x
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64, v1: i64, v2: i64, v3: i64, v4: i64):
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v5
CHECK-NEXT:     v6 = iconst.i64 0
CHECK-NEXT:     v7 = stack_addr.i64 ss0
CHECK-NEXT:     v8 = load.i64 v7
CHECK-NEXT:     v9 = isub v4, v1
CHECK-NEXT:     v10 = iconst.i64 1
CHECK-NEXT:     v11 = iadd v9, v10  ; v10 = 1
CHECK-NEXT:     v12 = iconst.i64 8
CHECK-NEXT:     v13 = isub v1, v1
CHECK-NEXT:     v14 = imul v13, v11
CHECK-NEXT:     v15 = isub v2, v3
CHECK-NEXT:     v16 = iadd v2, v14
CHECK-NEXT:     v17 = imul v16, v12  ; v12 = 8
CHECK-NEXT:     v18 = iadd v8, v17
CHECK-NEXT:     store v6, v18  ; v6 = 0
CHECK-NEXT:     v19 = iconst.i64 0
CHECK-NEXT:     v20 = stack_addr.i64 ss0
CHECK-NEXT:     v21 = load.i64 v20
CHECK-NEXT:     v22 = isub v4, v1
CHECK-NEXT:     v23 = iconst.i64 1
CHECK-NEXT:     v24 = iadd v22, v23  ; v23 = 1
CHECK-NEXT:     v25 = iconst.i64 8
CHECK-NEXT:     v26 = isub v3, v1
CHECK-NEXT:     v27 = imul v26, v24
CHECK-NEXT:     v28 = isub v4, v3
CHECK-NEXT:     v29 = iadd v4, v27
CHECK-NEXT:     v30 = imul v29, v25  ; v25 = 8
CHECK-NEXT:     v31 = iadd v21, v30
CHECK-NEXT:     store v19, v31  ; v19 = 0
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'foo' seems OK

CHECK: *** IR for 'foo2'
CHECK-NEXT: function u0:1(i64, i64, i64, i64, i64) system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; [indirect] x
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64, v1: i64, v2: i64, v3: i64, v4: i64):
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v5
CHECK-NEXT:     v6 = iconst.i64 0
CHECK-NEXT:     v7 = stack_addr.i64 ss0
CHECK-NEXT:     v8 = load.i64 v7
CHECK-NEXT:     v9 = isub v4, v1
CHECK-NEXT:     v10 = iconst.i64 1
CHECK-NEXT:     v11 = iadd v9, v10  ; v10 = 1
CHECK-NEXT:     v12 = iconst.i64 8
CHECK-NEXT:     v13 = isub v1, v1
CHECK-NEXT:     v14 = imul v13, v11
CHECK-NEXT:     v15 = isub v2, v3
CHECK-NEXT:     v16 = iadd v2, v14
CHECK-NEXT:     v17 = imul v16, v12  ; v12 = 8
CHECK-NEXT:     v18 = iadd v8, v17
CHECK-NEXT:     store v6, v18  ; v6 = 0
CHECK-NEXT:     v19 = iconst.i64 0
CHECK-NEXT:     v20 = stack_addr.i64 ss0
CHECK-NEXT:     v21 = load.i64 v20
CHECK-NEXT:     v22 = isub v4, v1
CHECK-NEXT:     v23 = iconst.i64 1
CHECK-NEXT:     v24 = iadd v22, v23  ; v23 = 1
CHECK-NEXT:     v25 = iconst.i64 8
CHECK-NEXT:     v26 = isub v3, v1
CHECK-NEXT:     v27 = imul v26, v24
CHECK-NEXT:     v28 = isub v4, v3
CHECK-NEXT:     v29 = iadd v4, v27
CHECK-NEXT:     v30 = imul v29, v25  ; v25 = 8
CHECK-NEXT:     v31 = iadd v21, v30
CHECK-NEXT:     store v19, v31  ; v19 = 0
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'foo2' seems OK

CHECK: *** IR for 'mymain1'
CHECK-NEXT: function u0:2() system_v {
CHECK-NEXT:     gv0 = symbol colocated userextname0 ; a
CHECK-NEXT:     sig0 = (i64, i64, i64, i64, i64) system_v
CHECK-NEXT:     fn0 = colocated u0:0 sig0 ; foo
CHECK-EMPTY:
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = global_value.i64 gv0
CHECK-NEXT:     v1 = iconst.i64 1
CHECK-NEXT:     v2 = iconst.i64 10
CHECK-NEXT:     v3 = iconst.i64 2
CHECK-NEXT:     v4 = iconst.i64 30
CHECK-NEXT:     call fn0(v0, v1, v2, v3, v4)  ; v1 = 1, v2 = 10, v3 = 2, v4 = 30
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'mymain1' seems OK

CHECK: *** IR for 'mymain2'
CHECK-NEXT: function u0:3() system_v {
CHECK-NEXT:     ss0 = explicit_slot 2320 ; [copy-in]
CHECK-NEXT:     gv0 = symbol colocated userextname0 ; a
CHECK-NEXT:     sig0 = (i64, i64, i64) -> i64 system_v
CHECK-NEXT:     sig1 = (i64, i64, i64, i64, i64) system_v
CHECK-NEXT:     fn0 = %Memcpy sig0
CHECK-NEXT:     fn1 = colocated u0:1 sig1 ; foo2
CHECK-EMPTY:
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = global_value.i64 gv0
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     v2 = iconst.i64 2320
CHECK-NEXT:     v3 = call fn0(v1, v0, v2)  ; v2 = 2320
CHECK-NEXT:     v4 = iconst.i64 1
CHECK-NEXT:     v5 = iconst.i64 10
CHECK-NEXT:     v6 = iconst.i64 2
CHECK-NEXT:     v7 = iconst.i64 30
CHECK-NEXT:     call fn1(v1, v4, v5, v6, v7)  ; v4 = 1, v5 = 10, v6 = 2, v7 = 30
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'mymain2' seems OK

}

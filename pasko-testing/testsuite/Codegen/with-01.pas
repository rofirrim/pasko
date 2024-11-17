{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}
program test;

type
 my_record1 = record
   a : real;
   b : integer;
 end;
 my_record2 = record
   x : real;
   y : integer;
   z : my_record1;
 end;

procedure foo;
var
    r2 : my_record2;
begin
    r2.x := 4.5;
    r2.y := 6;
    r2.z.a := 1.2;
    r2.z.b := 3;

    with r2, r2.z do
    begin
      x := x + 100;
      a := a + 100;
      b := b + 100;
    end;

    with r2, z do
    begin
      x := x + 100;
      a := a + 100;
      b := b + 100;
    end;
end;

begin
    foo;
end.

{

CHECK: *** IR for 'foo'
CHECK-NEXT: function u0:0() system_v {
CHECK-NEXT:     ss0 = explicit_slot 32 ; r2
CHECK-EMPTY: 
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = f64const 0x1.2000000000000p2
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v1  ; v0 = 0x1.2000000000000p2
CHECK-NEXT:     v2 = iconst.i64 6
CHECK-NEXT:     v3 = stack_addr.i64 ss0
CHECK-NEXT:     v4 = iconst.i64 8
CHECK-NEXT:     v5 = iadd v3, v4  ; v4 = 8
CHECK-NEXT:     store v2, v5  ; v2 = 6
CHECK-NEXT:     v6 = f64const 0x1.3333333333333p0
CHECK-NEXT:     v7 = stack_addr.i64 ss0
CHECK-NEXT:     v8 = iconst.i64 16
CHECK-NEXT:     v9 = iadd v7, v8  ; v8 = 16
CHECK-NEXT:     store v6, v9  ; v6 = 0x1.3333333333333p0
CHECK-NEXT:     v10 = iconst.i64 3
CHECK-NEXT:     v11 = stack_addr.i64 ss0
CHECK-NEXT:     v12 = iconst.i64 16
CHECK-NEXT:     v13 = iadd v11, v12  ; v12 = 16
CHECK-NEXT:     v14 = iconst.i64 8
CHECK-NEXT:     v15 = iadd v13, v14  ; v14 = 8
CHECK-NEXT:     store v10, v15  ; v10 = 3
CHECK-NEXT:     v16 = stack_addr.i64 ss0
CHECK-NEXT:     v17 = stack_addr.i64 ss0
CHECK-NEXT:     v18 = iconst.i64 16
CHECK-NEXT:     v19 = iadd v17, v18  ; v18 = 16
CHECK-NEXT:     v20 = load.f64 v16
CHECK-NEXT:     v21 = iconst.i64 100
CHECK-NEXT:     v22 = fcvt_from_sint.f64 v21  ; v21 = 100
CHECK-NEXT:     v23 = fadd v20, v22
CHECK-NEXT:     store v23, v16
CHECK-NEXT:     v24 = load.f64 v19
CHECK-NEXT:     v25 = iconst.i64 100
CHECK-NEXT:     v26 = fcvt_from_sint.f64 v25  ; v25 = 100
CHECK-NEXT:     v27 = fadd v24, v26
CHECK-NEXT:     store v27, v19
CHECK-NEXT:     v28 = iconst.i64 8
CHECK-NEXT:     v29 = iadd v19, v28  ; v28 = 8
CHECK-NEXT:     v30 = load.i64 v29
CHECK-NEXT:     v31 = iconst.i64 100
CHECK-NEXT:     v32 = iadd v30, v31  ; v31 = 100
CHECK-NEXT:     v33 = iconst.i64 8
CHECK-NEXT:     v34 = iadd v19, v33  ; v33 = 8
CHECK-NEXT:     store v32, v34
CHECK-NEXT:     v35 = stack_addr.i64 ss0
CHECK-NEXT:     v36 = iconst.i64 16
CHECK-NEXT:     v37 = iadd v35, v36  ; v36 = 16
CHECK-NEXT:     v38 = load.f64 v35
CHECK-NEXT:     v39 = iconst.i64 100
CHECK-NEXT:     v40 = fcvt_from_sint.f64 v39  ; v39 = 100
CHECK-NEXT:     v41 = fadd v38, v40
CHECK-NEXT:     store v41, v35
CHECK-NEXT:     v42 = load.f64 v37
CHECK-NEXT:     v43 = iconst.i64 100
CHECK-NEXT:     v44 = fcvt_from_sint.f64 v43  ; v43 = 100
CHECK-NEXT:     v45 = fadd v42, v44
CHECK-NEXT:     store v45, v37
CHECK-NEXT:     v46 = iconst.i64 8
CHECK-NEXT:     v47 = iadd v37, v46  ; v46 = 8
CHECK-NEXT:     v48 = load.i64 v47
CHECK-NEXT:     v49 = iconst.i64 100
CHECK-NEXT:     v50 = iadd v48, v49  ; v49 = 100
CHECK-NEXT:     v51 = iconst.i64 8
CHECK-NEXT:     v52 = iadd v37, v51  ; v51 = 8
CHECK-NEXT:     store v50, v52
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'foo' seems OK

}
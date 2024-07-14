{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main;

type
  my_record = record
     i: integer;
     x: real;
  end;

  my_record_2 = record
    b: boolean;
    r: my_record;
  end;

procedure foo_2;
var
  r: my_record_2;
begin
  r.b := true;
  r.r.i := 2;
  r.r.x := 3.4;
end;

begin
end.

{

CHECK:      *** IR for procedure 'foo_2'
CHECK-NEXT: function u0:38() system_v {
CHECK-NEXT:     ss0 = explicit_slot 24
CHECK-EMPTY:
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = iconst.i8 1
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     v2 = iconst.i64 0
CHECK-NEXT:     v3 = iadd v1, v2  ; v2 = 0
CHECK-NEXT:     store v0, v3  ; v0 = 1
CHECK-NEXT:     v4 = iconst.i64 2
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     v6 = iconst.i64 8
CHECK-NEXT:     v7 = iadd v5, v6  ; v6 = 8
CHECK-NEXT:     v8 = iconst.i64 0
CHECK-NEXT:     v9 = iadd v7, v8  ; v8 = 0
CHECK-NEXT:     store v4, v9  ; v4 = 2
CHECK-NEXT:     v10 = f64const 0x1.b333333333333p1
CHECK-NEXT:     v11 = stack_addr.i64 ss0
CHECK-NEXT:     v12 = iconst.i64 8
CHECK-NEXT:     v13 = iadd v11, v12  ; v12 = 8
CHECK-NEXT:     v14 = iconst.i64 8
CHECK-NEXT:     v15 = iadd v13, v14  ; v14 = 8
CHECK-NEXT:     store v10, v15  ; v10 = 0x1.b333333333333p1
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for procedure 'foo_2' seems OK

}

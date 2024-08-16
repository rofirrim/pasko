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

CHECK: *** IR for 'foo_2'
CHECK-NEXT: function u0:0() system_v {
CHECK-NEXT:     ss0 = explicit_slot 24
CHECK-EMPTY:
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = iconst.i8 1
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v1  ; v0 = 1
CHECK-NEXT:     v2 = iconst.i64 2
CHECK-NEXT:     v3 = stack_addr.i64 ss0
CHECK-NEXT:     v4 = iconst.i64 8
CHECK-NEXT:     v5 = iadd v3, v4  ; v4 = 8
CHECK-NEXT:     store v2, v5  ; v2 = 2
CHECK-NEXT:     v6 = f64const 0x1.b333333333333p1
CHECK-NEXT:     v7 = stack_addr.i64 ss0
CHECK-NEXT:     v8 = iconst.i64 8
CHECK-NEXT:     v9 = iadd v7, v8  ; v8 = 8
CHECK-NEXT:     v10 = iconst.i64 8
CHECK-NEXT:     v11 = iadd v9, v10  ; v10 = 8
CHECK-NEXT:     store v6, v11  ; v6 = 0x1.b333333333333p1
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'foo_2' seems OK

}

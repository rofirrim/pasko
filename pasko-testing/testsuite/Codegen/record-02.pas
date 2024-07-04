{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main;

var
  a: record
     b: boolean;
     i: integer;
     x: real;
     c: char;
  end;

begin
  a.b := true;
  a.i := 1;
  a.x := 2.3;
  a.c := 'A';
end.

{

CHECK: *** IR for main
CHECK-NEXT: function u0:21(i32, i64) -> i32 system_v {
CHECK-NEXT:     gv0 = symbol colocated userextname0
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i32, v1: i64):
CHECK-NEXT:     v2 = iconst.i8 1
CHECK-NEXT:     v3 = global_value.i64 gv0
CHECK-NEXT:     v4 = iconst.i64 0
CHECK-NEXT:     v5 = iadd v3, v4  ; v4 = 0
CHECK-NEXT:     store v2, v5  ; v2 = 1
CHECK-NEXT:     v6 = iconst.i64 1
CHECK-NEXT:     v7 = global_value.i64 gv0
CHECK-NEXT:     v8 = iconst.i64 8
CHECK-NEXT:     v9 = iadd v7, v8  ; v8 = 8
CHECK-NEXT:     store v6, v9  ; v6 = 1
CHECK-NEXT:     v10 = f64const 0x1.2666666666666p1
CHECK-NEXT:     v11 = global_value.i64 gv0
CHECK-NEXT:     v12 = iconst.i64 16
CHECK-NEXT:     v13 = iadd v11, v12  ; v12 = 16
CHECK-NEXT:     store v10, v13  ; v10 = 0x1.2666666666666p1
CHECK-NEXT:     v14 = iconst.i32 65
CHECK-NEXT:     v15 = global_value.i64 gv0
CHECK-NEXT:     v16 = iconst.i64 24
CHECK-NEXT:     v17 = iadd v15, v16  ; v16 = 24
CHECK-NEXT:     store v14, v17  ; v14 = 65
CHECK-NEXT:     v18 = iconst.i32 0
CHECK-NEXT:     return v18  ; v18 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for main seems OK

}

{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}
program main(output);

procedure level0;
var
  x0, x1: integer;

  procedure level1;
  var
    y0, y1 : integer;

    procedure level2;
    var
      z0, z1: integer;
    begin
      z0 := 6;
      z1 := 7;
      x0 := 308;
      x1 := 309;
      y0 := 210;
      y1 := 211;
      z0 := 112;
      z1 := 113;
    end;

  begin
    x0 := 202;
    x1 := 203;
    y0 := 104;
    y1 := 105;
    level2;
  end;

begin
  x0 := 100;
  x1 := 101;
  level1;
end;

begin
  level0;
end.

{

CHECK: *** IR for '_level0_level1_level2'
CHECK-NEXT: function u0:0(i64) system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; z0
CHECK-NEXT:     ss1 = explicit_slot 8 ; z1
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = iconst.i64 6
CHECK-NEXT:     v2 = stack_addr.i64 ss0
CHECK-NEXT:     store v1, v2  ; v1 = 6
CHECK-NEXT:     v3 = iconst.i64 7
CHECK-NEXT:     v4 = stack_addr.i64 ss1
CHECK-NEXT:     store v3, v4  ; v3 = 7
CHECK-NEXT:     v5 = iconst.i64 308
CHECK-NEXT:     v6 = load.i64 v0
CHECK-NEXT:     v7 = load.i64 v6+8
CHECK-NEXT:     store v5, v7  ; v5 = 308
CHECK-NEXT:     v8 = iconst.i64 309
CHECK-NEXT:     v9 = load.i64 v0
CHECK-NEXT:     v10 = load.i64 v9+16
CHECK-NEXT:     store v8, v10  ; v8 = 309
CHECK-NEXT:     v11 = iconst.i64 210
CHECK-NEXT:     v12 = load.i64 v0+8
CHECK-NEXT:     store v11, v12  ; v11 = 210
CHECK-NEXT:     v13 = iconst.i64 211
CHECK-NEXT:     v14 = load.i64 v0+16
CHECK-NEXT:     store v13, v14  ; v13 = 211
CHECK-NEXT:     v15 = iconst.i64 112
CHECK-NEXT:     v16 = stack_addr.i64 ss0
CHECK-NEXT:     store v15, v16  ; v15 = 112
CHECK-NEXT:     v17 = iconst.i64 113
CHECK-NEXT:     v18 = stack_addr.i64 ss1
CHECK-NEXT:     store v17, v18  ; v17 = 113
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for '_level0_level1_level2' seems OK

CHECK: *** IR for '_level0_level1'
CHECK-NEXT: function u0:1(i64) system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; y0
CHECK-NEXT:     ss1 = explicit_slot 8 ; y1
CHECK-NEXT:     ss2 = explicit_slot 24 ; [nested-environment]
CHECK-NEXT:     sig0 = (i64) system_v
CHECK-NEXT:     fn0 = colocated u0:0 sig0 ; _level0_level1_level2
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = iconst.i64 202
CHECK-NEXT:     v2 = load.i64 v0+8
CHECK-NEXT:     store v1, v2  ; v1 = 202
CHECK-NEXT:     v3 = iconst.i64 203
CHECK-NEXT:     v4 = load.i64 v0+16
CHECK-NEXT:     store v3, v4  ; v3 = 203
CHECK-NEXT:     v5 = iconst.i64 104
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     store v5, v6  ; v5 = 104
CHECK-NEXT:     v7 = iconst.i64 105
CHECK-NEXT:     v8 = stack_addr.i64 ss1
CHECK-NEXT:     store v7, v8  ; v7 = 105
CHECK-NEXT:     v9 = iconst.i64 0
CHECK-NEXT:     stack_store v0, ss2
CHECK-NEXT:     v10 = stack_addr.i64 ss0
CHECK-NEXT:     stack_store v10, ss2+8
CHECK-NEXT:     v11 = stack_addr.i64 ss1
CHECK-NEXT:     stack_store v11, ss2+16
CHECK-NEXT:     v12 = stack_addr.i64 ss2
CHECK-NEXT:     call fn0(v12)
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for '_level0_level1' seems OK

CHECK: *** IR for 'level0'
CHECK-NEXT: function u0:2() system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; x0
CHECK-NEXT:     ss1 = explicit_slot 8 ; x1
CHECK-NEXT:     ss2 = explicit_slot 24 ; [nested-environment]
CHECK-NEXT:     sig0 = (i64) system_v
CHECK-NEXT:     fn0 = colocated u0:1 sig0 ; _level0_level1
CHECK-EMPTY:
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = iconst.i64 100
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v1  ; v0 = 100
CHECK-NEXT:     v2 = iconst.i64 101
CHECK-NEXT:     v3 = stack_addr.i64 ss1
CHECK-NEXT:     store v2, v3  ; v2 = 101
CHECK-NEXT:     v4 = iconst.i64 0
CHECK-NEXT:     stack_store v4, ss2  ; v4 = 0
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     stack_store v5, ss2+8
CHECK-NEXT:     v6 = stack_addr.i64 ss1
CHECK-NEXT:     stack_store v6, ss2+16
CHECK-NEXT:     v7 = stack_addr.i64 ss2
CHECK-NEXT:     call fn0(v7)
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'level0' seems OK

}

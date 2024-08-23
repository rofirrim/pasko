{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main;
type
  myrecord = record
    a, b : integer;
  end;
  mypointer = ^integer;

procedure sink_1(a: mypointer);
var
  x : mypointer;
begin
  x := a;
end;

procedure source_1;
var
  x : mypointer;
begin
  sink_1(x);
end;

procedure sink_2(a: myrecord);
var
  x : myrecord;
begin
  x.b := a.b;
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

CHECK: *** IR for 'sink_1'
CHECK-NEXT: function u0:0(i64) system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; a
CHECK-NEXT:     ss1 = explicit_slot 8 ; x
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss0
CHECK-NEXT:     v3 = load.i64 v2
CHECK-NEXT:     v4 = stack_addr.i64 ss1
CHECK-NEXT:     store v3, v4
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'sink_1' seems OK

CHECK: *** IR for 'source_1'
CHECK-NEXT: function u0:1() system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; x
CHECK-NEXT:     sig0 = (i64) system_v
CHECK-NEXT:     fn0 = colocated u0:0 sig0 ; sink_1
CHECK-EMPTY:
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = stack_addr.i64 ss0
CHECK-NEXT:     v1 = load.i64 v0
CHECK-NEXT:     call fn0(v1)
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'source_1' seems OK

CHECK: *** IR for 'sink_2'
CHECK-NEXT: function u0:2(i64) system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; [indirect] a
CHECK-NEXT:     ss1 = explicit_slot 16 ; x
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss0
CHECK-NEXT:     v3 = load.i64 v2
CHECK-NEXT:     v4 = iconst.i64 8
CHECK-NEXT:     v5 = iadd v3, v4  ; v4 = 8
CHECK-NEXT:     v6 = load.i64 v5
CHECK-NEXT:     v7 = stack_addr.i64 ss1
CHECK-NEXT:     v8 = iconst.i64 8
CHECK-NEXT:     v9 = iadd v7, v8  ; v8 = 8
CHECK-NEXT:     store v6, v9
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'sink_2' seems OK

CHECK: *** IR for 'source_2'
CHECK-NEXT: function u0:3() system_v {
CHECK-NEXT:     ss0 = explicit_slot 16 ; x
CHECK-NEXT:     ss1 = explicit_slot 16 ; [copy-in]
CHECK-NEXT:     sig0 = (i64, i64, i64) -> i64 system_v
CHECK-NEXT:     sig1 = (i64) system_v
CHECK-NEXT:     fn0 = %Memcpy sig0
CHECK-NEXT:     fn1 = colocated u0:2 sig1 ; sink_2
CHECK-EMPTY:
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = stack_addr.i64 ss0
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     v2 = iconst.i64 16
CHECK-NEXT:     v3 = call fn0(v1, v0, v2)  ; v2 = 16
CHECK-NEXT:     call fn1(v1)
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'source_2' seems OK

}

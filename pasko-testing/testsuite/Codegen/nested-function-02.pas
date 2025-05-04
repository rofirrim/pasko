{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}
program main(output);
type
  my_pointer_integer = ^integer;
var
  q: my_pointer_integer;

procedure test(one_integer: integer;
              function one_function : integer;
              p: my_pointer_integer);
var
  x: integer;
begin
  x := one_integer + one_function;
  p^ := x;
end;

function my_function: integer;
begin
 my_function := 3;
end;

procedure foo;
begin
  new(q);
  test(
     my_function,  { calls my_function, which evaluates to 3 }
     my_function,  { passes my_function as a function }
     q
     );
  writeln(q^);     { writes 6 }
  dispose(q);
end;

begin
  foo;
end.

{

CHECK: *** IR for 'test'
CHECK-NEXT: function u0:0(i64, i64, i64, i64) system_v {
CHECK-NEXT:     ss0 = explicit_slot 16, align = 8 ; [function] one_function
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-NEXT:     sig0 = (i64) -> i64 system_v
CHECK-NEXT:     sig1 = () -> i64 system_v
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64, v1: i64, v2: i64, v3: i64):
CHECK-NEXT:     v16 -> v3
CHECK-NEXT:     stack_store v1, ss0
CHECK-NEXT:     stack_store v2, ss0+8
CHECK-NEXT:     v4 = stack_addr.i64 ss0
CHECK-NEXT:     v5 = load.i64 v4+8
CHECK-NEXT:     v6 = iconst.i64 0
CHECK-NEXT:     v7 = icmp ne v5, v6  ; v6 = 0
CHECK-NEXT:     v8 = stack_addr.i64 ss1
CHECK-NEXT:     brif v7, block1, block3
CHECK-EMPTY:
CHECK-NEXT: block1:
CHECK-NEXT:     v9 = load.i64 v4
CHECK-NEXT:     v10 = call_indirect sig0, v9(v5)
CHECK-NEXT:     store v10, v8
CHECK-NEXT:     jump block2(v3)
CHECK-EMPTY:
CHECK-NEXT: block3:
CHECK-NEXT:     v11 = load.i64 v4
CHECK-NEXT:     v12 = call_indirect sig1, v11()
CHECK-NEXT:     store v12, v8
CHECK-NEXT:     jump block2(v16)
CHECK-EMPTY:
CHECK-NEXT: block2(v15: i64):
CHECK-NEXT:     v13 = load.i64 v8
CHECK-NEXT:     ! v0 → one_integer 
CHECK-NEXT:     v14 = iadd.i64 v0, v13
CHECK-NEXT:     ! x ← v14 
CHECK-NEXT:     ! v14 → x 
CHECK-NEXT:     store v14, v15
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'test' seems OK

CHECK: *** IR for 'my_function'
CHECK-NEXT: function u0:1() -> i64 system_v {
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = iconst.i64 3
CHECK-NEXT:     ! my_function ← v0 
CHECK-NEXT:     ! v0 → my_function 
CHECK-NEXT:     return v0  ; v0 = 3
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_function' seems OK
CHECK-EMPTY:
CHECK-NEXT: *** IR for 'foo'
CHECK-NEXT: function u0:2() system_v {
CHECK-NEXT:     gv0 = symbol colocated userextname0 ; q
CHECK-NEXT:     gv1 = symbol colocated userextname4 ; [output-textfile]
CHECK-NEXT:     sig0 = (i64) -> i64 system_v
CHECK-NEXT:     sig1 = () -> i64 system_v
CHECK-NEXT:     sig2 = (i64, i64, i64, i64) system_v
CHECK-NEXT:     sig3 = (i64, i64, i64) system_v
CHECK-NEXT:     sig4 = (i64) system_v
CHECK-NEXT:     sig5 = (i64) system_v
CHECK-NEXT:     fn0 = u0:3 sig0 ; __pasko_pointer_new
CHECK-NEXT:     fn1 = colocated u0:1 sig1 ; my_function
CHECK-NEXT:     fn2 = colocated u0:0 sig2 ; test
CHECK-NEXT:     fn3 = u0:4 sig3 ; __pasko_write_textfile_i64
CHECK-NEXT:     fn4 = u0:5 sig4 ; __pasko_write_textfile_newline
CHECK-NEXT:     fn5 = u0:6 sig5 ; __pasko_pointer_dispose
CHECK-EMPTY:
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = global_value.i64 gv0
CHECK-NEXT:     v1 = iconst.i64 8
CHECK-NEXT:     v2 = call fn0(v1)  ; v1 = 8
CHECK-NEXT:     store v2, v0
CHECK-NEXT:     v3 = call fn1()
CHECK-NEXT:     v4 = func_addr.i64 fn1
CHECK-NEXT:     v5 = global_value.i64 gv0
CHECK-NEXT:     v6 = load.i64 v5
CHECK-NEXT:     v7 = iconst.i64 0
CHECK-NEXT:     call fn2(v3, v4, v7, v6)  ; v7 = 0
CHECK-NEXT:     v8 = global_value.i64 gv1
CHECK-NEXT:     v9 = load.i64 v8
CHECK-NEXT:     v10 = global_value.i64 gv0
CHECK-NEXT:     v11 = load.i64 v10
CHECK-NEXT:     v12 = load.i64 v11
CHECK-NEXT:     v13 = iconst.i64 0
CHECK-NEXT:     call fn3(v9, v12, v13)  ; v13 = 0
CHECK-NEXT:     call fn4(v9)
CHECK-NEXT:     v14 = global_value.i64 gv0
CHECK-NEXT:     v15 = load.i64 v14
CHECK-NEXT:     call fn5(v15)
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'foo' seems OK

}

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
CHECK-NEXT:     ss0 = explicit_slot 8 ; one_integer
CHECK-NEXT:     ss1 = explicit_slot 16 ; [function] one_function
CHECK-NEXT:     ss2 = explicit_slot 8 ; p
CHECK-NEXT:     ss3 = explicit_slot 8 ; x
CHECK-NEXT:     ss4 = explicit_slot 8
CHECK-NEXT:     sig0 = (i64) -> i64 system_v
CHECK-NEXT:     sig1 = () -> i64 system_v
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64, v1: i64, v2: i64, v3: i64):
CHECK-NEXT:     v4 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v4
CHECK-NEXT:     v5 = stack_addr.i64 ss1
CHECK-NEXT:     store v1, v5
CHECK-NEXT:     store v2, v5+8
CHECK-NEXT:     v6 = stack_addr.i64 ss2
CHECK-NEXT:     store v3, v6
CHECK-NEXT:     v7 = stack_addr.i64 ss0
CHECK-NEXT:     v8 = load.i64 v7
CHECK-NEXT:     v9 = stack_addr.i64 ss1
CHECK-NEXT:     v10 = load.i64 v9+8
CHECK-NEXT:     v11 = iconst.i64 0
CHECK-NEXT:     v12 = icmp ne v10, v11  ; v11 = 0
CHECK-NEXT:     v13 = stack_addr.i64 ss4
CHECK-NEXT:     brif v12, block1, block3
CHECK-EMPTY:
CHECK-NEXT: block1:
CHECK-NEXT:     v14 = load.i64 v9
CHECK-NEXT:     v15 = call_indirect sig0, v14(v10)
CHECK-NEXT:     store v15, v13
CHECK-NEXT:     jump block2
CHECK-EMPTY:
CHECK-NEXT: block3:
CHECK-NEXT:     v16 = load.i64 v9
CHECK-NEXT:     v17 = call_indirect sig1, v16()
CHECK-NEXT:     store v17, v13
CHECK-NEXT:     jump block2
CHECK-EMPTY:
CHECK-NEXT: block2:
CHECK-NEXT:     v18 = load.i64 v13
CHECK-NEXT:     v19 = iadd.i64 v8, v18
CHECK-NEXT:     v20 = stack_addr.i64 ss3
CHECK-NEXT:     store v19, v20
CHECK-NEXT:     v21 = stack_addr.i64 ss3
CHECK-NEXT:     v22 = load.i64 v21
CHECK-NEXT:     v23 = stack_addr.i64 ss2
CHECK-NEXT:     v24 = load.i64 v23
CHECK-NEXT:     store v22, v24
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'test' seems OK

CHECK: *** IR for 'my_function'
CHECK-NEXT: function u0:1() -> i64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; my_function
CHECK-EMPTY:
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = iconst.i64 3
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v1  ; v0 = 3
CHECK-NEXT:     v2 = stack_addr.i64 ss0
CHECK-NEXT:     v3 = load.i64 v2
CHECK-NEXT:     return v3
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_function' seems OK

CHECK: *** IR for 'foo'
CHECK-NEXT: function u0:2() system_v {
CHECK-NEXT:     gv0 = symbol colocated userextname0 ; q
CHECK-NEXT:     gv1 = symbol colocated userextname4 ; [output-textfile]
CHECK-NEXT:     sig0 = (i64, i64) system_v
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
CHECK-NEXT:     call fn0(v0, v1)  ; v1 = 8
CHECK-NEXT:     v2 = call fn1()
CHECK-NEXT:     v3 = func_addr.i64 fn1
CHECK-NEXT:     v4 = global_value.i64 gv0
CHECK-NEXT:     v5 = load.i64 v4
CHECK-NEXT:     v6 = iconst.i64 0
CHECK-NEXT:     call fn2(v2, v3, v6, v5)  ; v6 = 0
CHECK-NEXT:     v7 = global_value.i64 gv1
CHECK-NEXT:     v8 = load.i64 v7
CHECK-NEXT:     v9 = global_value.i64 gv0
CHECK-NEXT:     v10 = load.i64 v9
CHECK-NEXT:     v11 = load.i64 v10
CHECK-NEXT:     v12 = iconst.i64 0
CHECK-NEXT:     call fn3(v8, v11, v12)  ; v12 = 0
CHECK-NEXT:     call fn4(v8)
CHECK-NEXT:     v13 = global_value.i64 gv0
CHECK-NEXT:     call fn5(v13)
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'foo' seems OK

}

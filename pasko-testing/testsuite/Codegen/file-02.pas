{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program test(output);

procedure foo(var f: text);
begin
  writeln(f, 'Hello world');
end;

begin
  foo(output);
end.

{

CHECK: *** IR for 'foo'
CHECK-NEXT: function u0:0(i64) system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; [indirect] f
CHECK-NEXT:     gv0 = symbol colocated userextname0 ; [string: 'Hello world']
CHECK-NEXT:     sig0 = (i64, i64, i64) system_v
CHECK-NEXT:     sig1 = (i64) system_v
CHECK-NEXT:     fn0 = u0:1 sig0 ; __pasko_write_textfile_str
CHECK-NEXT:     fn1 = u0:2 sig1 ; __pasko_write_textfile_newline
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss0
CHECK-NEXT:     v3 = load.i64 v2
CHECK-NEXT:     v4 = load.i64 v3
CHECK-NEXT:     v5 = global_value.i64 gv0
CHECK-NEXT:     v6 = iconst.i64 11
CHECK-NEXT:     call fn0(v4, v5, v6)  ; v6 = 11
CHECK-NEXT:     call fn1(v4)
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'foo' seems OK

CHECK: *** IR for 'main'
CHECK-NEXT: function u0:3(i32, i64) -> i32 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; [null-ended-array: program-parameter-names]
CHECK-NEXT:     ss1 = explicit_slot 8 ; [null-ended-array: global-files]
CHECK-NEXT:     gv0 = symbol colocated userextname4 ; [input-textfile]
CHECK-NEXT:     gv1 = symbol colocated userextname5 ; [output-textfile]
CHECK-NEXT:     sig0 = (i32, i64, i32, i64, i32, i64) system_v
CHECK-NEXT:     sig1 = (i32, i64) system_v
CHECK-NEXT:     sig2 = () -> i64 system_v
CHECK-NEXT:     sig3 = () -> i64 system_v
CHECK-NEXT:     sig4 = (i64) system_v
CHECK-NEXT:     fn0 = u0:4 sig0 ; __pasko_init
CHECK-NEXT:     fn1 = u0:5 sig1 ; __pasko_finish
CHECK-NEXT:     fn2 = u0:6 sig2 ; __pasko_get_input
CHECK-NEXT:     fn3 = u0:7 sig3 ; __pasko_get_output
CHECK-NEXT:     fn4 = colocated u0:0 sig4 ; foo
CHECK-EMPTY: 
CHECK-NEXT: block0(v0: i32, v1: i64):
CHECK-NEXT:     v2 = iconst.i32 0
CHECK-NEXT:     v3 = iconst.i64 0
CHECK-NEXT:     stack_store v3, ss0  ; v3 = 0
CHECK-NEXT:     v4 = stack_addr.i64 ss0
CHECK-NEXT:     v5 = iconst.i64 0
CHECK-NEXT:     stack_store v5, ss1  ; v5 = 0
CHECK-NEXT:     v6 = stack_addr.i64 ss1
CHECK-NEXT:     v7 = iconst.i32 0
CHECK-NEXT:     call fn0(v0, v1, v2, v4, v7, v6)  ; v2 = 0, v7 = 0
CHECK-NEXT:     v8 = call fn2()
CHECK-NEXT:     v9 = global_value.i64 gv0
CHECK-NEXT:     store v8, v9
CHECK-NEXT:     v10 = call fn3()
CHECK-NEXT:     v11 = global_value.i64 gv1
CHECK-NEXT:     store v10, v11
CHECK-NEXT:     v12 = global_value.i64 gv1
CHECK-NEXT:     call fn4(v12)
CHECK-NEXT:     call fn1(v7, v6)  ; v7 = 0
CHECK-NEXT:     v13 = iconst.i32 0
CHECK-NEXT:     return v13  ; v13 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'main' seems OK

}

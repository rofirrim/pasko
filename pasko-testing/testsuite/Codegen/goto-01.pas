{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main(output);

procedure foo;
label 1000;
var
  i : integer;
begin
  i := 0;
  1000:
    writeln('hello world = ', i);
    i := i + 1;
    if i < 10 then goto 1000;
  writeln('done!');
end;

begin
  foo;
end.

{

CHECK: *** IR for 'foo'
CHECK-NEXT: function u0:0() system_v {
CHECK-NEXT:     ss0 = explicit_slot 8 ; i
CHECK-NEXT:     gv0 = symbol colocated userextname0 ; [output-textfile]
CHECK-NEXT:     gv1 = symbol colocated userextname1 ; [string: 'hello world = ']
CHECK-NEXT:     gv2 = symbol colocated userextname5 ; [string: 'done!']
CHECK-NEXT:     sig0 = (i64, i64, i64) system_v
CHECK-NEXT:     sig1 = (i64, i64, i64) system_v
CHECK-NEXT:     sig2 = (i64) system_v
CHECK-NEXT:     fn0 = u0:1 sig0 ; __pasko_write_textfile_str
CHECK-NEXT:     fn1 = u0:2 sig1 ; __pasko_write_textfile_i64
CHECK-NEXT:     fn2 = u0:3 sig2 ; __pasko_write_textfile_newline
CHECK-EMPTY:
CHECK-NEXT: block0:
CHECK-NEXT:     v0 = iconst.i64 0
CHECK-NEXT:     v1 = stack_addr.i64 ss0
CHECK-NEXT:     store v0, v1  ; v0 = 0
CHECK-NEXT:     jump block1
CHECK-EMPTY:
CHECK-NEXT: block1:
CHECK-NEXT:     v2 = global_value.i64 gv0
CHECK-NEXT:     v3 = load.i64 v2
CHECK-NEXT:     v4 = global_value.i64 gv1
CHECK-NEXT:     v5 = iconst.i64 14
CHECK-NEXT:     call fn0(v3, v4, v5)  ; v5 = 14
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.i64 v6
CHECK-NEXT:     v8 = iconst.i64 0
CHECK-NEXT:     call fn1(v3, v7, v8)  ; v8 = 0
CHECK-NEXT:     call fn2(v3)
CHECK-NEXT:     v9 = stack_addr.i64 ss0
CHECK-NEXT:     v10 = load.i64 v9
CHECK-NEXT:     v11 = iconst.i64 1
CHECK-NEXT:     v12 = iadd v10, v11  ; v11 = 1
CHECK-NEXT:     v13 = stack_addr.i64 ss0
CHECK-NEXT:     store v12, v13
CHECK-NEXT:     v14 = stack_addr.i64 ss0
CHECK-NEXT:     v15 = load.i64 v14
CHECK-NEXT:     v16 = iconst.i64 10
CHECK-NEXT:     v17 = icmp slt v15, v16  ; v16 = 10
CHECK-NEXT:     brif v17, block2, block3
CHECK-EMPTY:
CHECK-NEXT: block2:
CHECK-NEXT:     jump block1
CHECK-EMPTY:
CHECK-NEXT: block4:
CHECK-NEXT:     jump block3
CHECK-EMPTY:
CHECK-NEXT: block3:
CHECK-NEXT:     v18 = global_value.i64 gv0
CHECK-NEXT:     v19 = load.i64 v18
CHECK-NEXT:     v20 = global_value.i64 gv2
CHECK-NEXT:     v21 = iconst.i64 5
CHECK-NEXT:     call fn0(v19, v20, v21)  ; v21 = 5
CHECK-NEXT:     call fn2(v19)
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'foo' seems OK

}

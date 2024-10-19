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
CHECK-NEXT:     ! i ← v0 
CHECK-NEXT:     jump block1(v0)  ; v0 = 0
CHECK-EMPTY:
CHECK-NEXT: block1(v5: i64):
CHECK-NEXT:     v1 = global_value.i64 gv0
CHECK-NEXT:     v2 = load.i64 v1
CHECK-NEXT:     v3 = global_value.i64 gv1
CHECK-NEXT:     v4 = iconst.i64 14
CHECK-NEXT:     call fn0(v2, v3, v4)  ; v4 = 14
CHECK-NEXT:     v6 = iconst.i64 0
CHECK-NEXT:     ! v5 → i 
CHECK-NEXT:     call fn1(v2, v5, v6)  ; v6 = 0
CHECK-NEXT:     call fn2(v2)
CHECK-NEXT:     v7 = iconst.i64 1
CHECK-NEXT:     ! v5 → i 
CHECK-NEXT:     v8 = iadd v5, v7  ; v7 = 1
CHECK-NEXT:     v15 -> v8
CHECK-NEXT:     ! i ← v8 
CHECK-NEXT:     v9 = iconst.i64 10
CHECK-NEXT:     ! v8 → i 
CHECK-NEXT:     v10 = icmp slt v8, v9  ; v9 = 10
CHECK-NEXT:     brif v10, block2, block3
CHECK-EMPTY:
CHECK-NEXT: block2:
CHECK-NEXT:     jump block1(v15)
CHECK-EMPTY:
CHECK-NEXT: block4:
CHECK-NEXT:     jump block3
CHECK-EMPTY:
CHECK-NEXT: block3:
CHECK-NEXT:     v11 = global_value.i64 gv0
CHECK-NEXT:     v12 = load.i64 v11
CHECK-NEXT:     v13 = global_value.i64 gv2
CHECK-NEXT:     v14 = iconst.i64 5
CHECK-NEXT:     call fn0(v12, v13, v14)  ; v14 = 5
CHECK-NEXT:     call fn2(v12)
CHECK-NEXT:     return
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'foo' seems OK

}

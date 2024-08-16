{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program test(my_file, input, my_text);

var
  my_file: file of real;
  my_text: text;
  b : boolean;
begin
  b := eof(my_file);
  b := eof(my_text);
  b := eoln(my_text);

  b := eoln;
  b := eof;
end.

{

CHECK: *** IR for 'main'
CHECK-NEXT: function u0:0(i32, i64) -> i32 system_v {
CHECK-NEXT:     ss0 = explicit_slot 24
CHECK-NEXT:     ss1 = explicit_slot 24
CHECK-NEXT:     gv0 = symbol colocated userextname4
CHECK-NEXT:     gv1 = symbol colocated userextname5
CHECK-NEXT:     gv2 = symbol colocated userextname6
CHECK-NEXT:     gv3 = symbol colocated userextname7
CHECK-NEXT:     gv4 = symbol colocated userextname8
CHECK-NEXT:     gv5 = symbol colocated userextname9
CHECK-NEXT:     gv6 = symbol colocated userextname11
CHECK-NEXT:     sig0 = (i32, i64, i32, i64, i32, i64) system_v
CHECK-NEXT:     sig1 = (i32, i64) system_v
CHECK-NEXT:     sig2 = () -> i64 system_v
CHECK-NEXT:     sig3 = () -> i64 system_v
CHECK-NEXT:     sig4 = (i64) -> i8 system_v
CHECK-NEXT:     sig5 = (i64) -> i8 system_v
CHECK-NEXT:     sig6 = (i64) -> i8 system_v
CHECK-NEXT:     fn0 = u0:1 sig0
CHECK-NEXT:     fn1 = u0:2 sig1
CHECK-NEXT:     fn2 = u0:3 sig2
CHECK-NEXT:     fn3 = u0:4 sig3
CHECK-NEXT:     fn4 = u0:5 sig4
CHECK-NEXT:     fn5 = u0:6 sig5
CHECK-NEXT:     fn6 = u0:7 sig6
CHECK-EMPTY: 
CHECK-NEXT: block0(v0: i32, v1: i64):
CHECK-NEXT:     v2 = iconst.i32 2
CHECK-NEXT:     v3 = global_value.i64 gv0
CHECK-NEXT:     v4 = global_value.i64 gv1
CHECK-NEXT:     stack_store v3, ss0
CHECK-NEXT:     stack_store v4, ss0+8
CHECK-NEXT:     v5 = iconst.i64 0
CHECK-NEXT:     stack_store v5, ss0+16  ; v5 = 0
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = global_value.i64 gv2
CHECK-NEXT:     v8 = global_value.i64 gv3
CHECK-NEXT:     stack_store v7, ss1
CHECK-NEXT:     stack_store v8, ss1+8
CHECK-NEXT:     v9 = iconst.i64 0
CHECK-NEXT:     stack_store v9, ss1+16  ; v9 = 0
CHECK-NEXT:     v10 = stack_addr.i64 ss1
CHECK-NEXT:     v11 = iconst.i32 2
CHECK-NEXT:     call fn0(v0, v1, v2, v6, v11, v10)  ; v2 = 2, v11 = 2
CHECK-NEXT:     v12 = call fn2()
CHECK-NEXT:     v13 = global_value.i64 gv4
CHECK-NEXT:     store v12, v13
CHECK-NEXT:     v14 = call fn3()
CHECK-NEXT:     v15 = global_value.i64 gv5
CHECK-NEXT:     store v14, v15
CHECK-NEXT:     v16 = global_value.i64 gv2
CHECK-NEXT:     v17 = load.i64 v16
CHECK-NEXT:     v18 = call fn4(v17)
CHECK-NEXT:     v19 = global_value.i64 gv6
CHECK-NEXT:     store v18, v19
CHECK-NEXT:     v20 = global_value.i64 gv3
CHECK-NEXT:     v21 = load.i64 v20
CHECK-NEXT:     v22 = call fn5(v21)
CHECK-NEXT:     v23 = global_value.i64 gv6
CHECK-NEXT:     store v22, v23
CHECK-NEXT:     v24 = global_value.i64 gv3
CHECK-NEXT:     v25 = load.i64 v24
CHECK-NEXT:     v26 = call fn6(v25)
CHECK-NEXT:     v27 = global_value.i64 gv6
CHECK-NEXT:     store v26, v27
CHECK-NEXT:     v28 = global_value.i64 gv4
CHECK-NEXT:     v29 = load.i64 v28
CHECK-NEXT:     v30 = call fn6(v29)
CHECK-NEXT:     v31 = global_value.i64 gv6
CHECK-NEXT:     store v30, v31
CHECK-NEXT:     v32 = global_value.i64 gv4
CHECK-NEXT:     v33 = load.i64 v32
CHECK-NEXT:     v34 = call fn5(v33)
CHECK-NEXT:     v35 = global_value.i64 gv6
CHECK-NEXT:     store v34, v35
CHECK-NEXT:     call fn1(v11, v10)  ; v11 = 2
CHECK-NEXT:     v36 = iconst.i32 0
CHECK-NEXT:     return v36  ; v36 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'main' seems OK

}

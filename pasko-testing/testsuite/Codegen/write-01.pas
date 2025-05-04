{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program main(output);

begin
  write(true);
  write(false);

  write('hello');

  write(1);
  write(1:10);

  write(1.23);
  write(1.23:10);
  write(1.23:10:3);
end.

{

CHECK: *** IR for 'main'
CHECK-NEXT: function u0:0(i32, i64) -> i32 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8, align = 8 ; [null-ended-array: program-parameter-names]
CHECK-NEXT:     ss1 = explicit_slot 8, align = 8 ; [null-ended-array: global-files]
CHECK-NEXT:     gv0 = symbol colocated userextname4 ; [input-textfile]
CHECK-NEXT:     gv1 = symbol colocated userextname5 ; [output-textfile]
CHECK-NEXT:     gv2 = symbol colocated userextname7 ; [string: 'hello']
CHECK-NEXT:     sig0 = (i32, i64, i32, i64, i32, i64) system_v
CHECK-NEXT:     sig1 = (i32, i64) system_v
CHECK-NEXT:     sig2 = () -> i64 system_v
CHECK-NEXT:     sig3 = () -> i64 system_v
CHECK-NEXT:     sig4 = (i64, i8) system_v
CHECK-NEXT:     sig5 = (i64, i64, i64) system_v
CHECK-NEXT:     sig6 = (i64, i64, i64) system_v
CHECK-NEXT:     sig7 = (i64, f64, i64, i64) system_v
CHECK-NEXT:     fn0 = u0:1 sig0 ; __pasko_init
CHECK-NEXT:     fn1 = u0:2 sig1 ; __pasko_finish
CHECK-NEXT:     fn2 = u0:3 sig2 ; __pasko_get_input
CHECK-NEXT:     fn3 = u0:4 sig3 ; __pasko_get_output
CHECK-NEXT:     fn4 = u0:5 sig4 ; __pasko_write_textfile_bool
CHECK-NEXT:     fn5 = u0:6 sig5 ; __pasko_write_textfile_str
CHECK-NEXT:     fn6 = u0:7 sig6 ; __pasko_write_textfile_i64
CHECK-NEXT:     fn7 = u0:8 sig7 ; __pasko_write_textfile_f64
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
CHECK-NEXT:     v13 = load.i64 v12
CHECK-NEXT:     v14 = iconst.i8 1
CHECK-NEXT:     call fn4(v13, v14)  ; v14 = 1
CHECK-NEXT:     v15 = global_value.i64 gv1
CHECK-NEXT:     v16 = load.i64 v15
CHECK-NEXT:     v17 = iconst.i8 0
CHECK-NEXT:     call fn4(v16, v17)  ; v17 = 0
CHECK-NEXT:     v18 = global_value.i64 gv1
CHECK-NEXT:     v19 = load.i64 v18
CHECK-NEXT:     v20 = global_value.i64 gv2
CHECK-NEXT:     v21 = iconst.i64 5
CHECK-NEXT:     call fn5(v19, v20, v21)  ; v21 = 5
CHECK-NEXT:     v22 = global_value.i64 gv1
CHECK-NEXT:     v23 = load.i64 v22
CHECK-NEXT:     v24 = iconst.i64 1
CHECK-NEXT:     v25 = iconst.i64 0
CHECK-NEXT:     call fn6(v23, v24, v25)  ; v24 = 1, v25 = 0
CHECK-NEXT:     v26 = global_value.i64 gv1
CHECK-NEXT:     v27 = load.i64 v26
CHECK-NEXT:     v28 = iconst.i64 1
CHECK-NEXT:     v29 = iconst.i64 10
CHECK-NEXT:     call fn6(v27, v28, v29)  ; v28 = 1, v29 = 10
CHECK-NEXT:     v30 = global_value.i64 gv1
CHECK-NEXT:     v31 = load.i64 v30
CHECK-NEXT:     v32 = f64const 0x1.3ae147ae147aep0
CHECK-NEXT:     v33 = iconst.i64 0
CHECK-NEXT:     v34 = iconst.i64 0
CHECK-NEXT:     call fn7(v31, v32, v33, v34)  ; v32 = 0x1.3ae147ae147aep0, v33 = 0, v34 = 0
CHECK-NEXT:     v35 = global_value.i64 gv1
CHECK-NEXT:     v36 = load.i64 v35
CHECK-NEXT:     v37 = f64const 0x1.3ae147ae147aep0
CHECK-NEXT:     v38 = iconst.i64 10
CHECK-NEXT:     v39 = iconst.i64 0
CHECK-NEXT:     call fn7(v36, v37, v38, v39)  ; v37 = 0x1.3ae147ae147aep0, v38 = 10, v39 = 0
CHECK-NEXT:     v40 = global_value.i64 gv1
CHECK-NEXT:     v41 = load.i64 v40
CHECK-NEXT:     v42 = f64const 0x1.3ae147ae147aep0
CHECK-NEXT:     v43 = iconst.i64 10
CHECK-NEXT:     v44 = iconst.i64 3
CHECK-NEXT:     call fn7(v41, v42, v43, v44)  ; v42 = 0x1.3ae147ae147aep0, v43 = 10, v44 = 3
CHECK-NEXT:     call fn1(v7, v6)  ; v7 = 0
CHECK-NEXT:     v45 = iconst.i32 0
CHECK-NEXT:     return v45  ; v45 = 0
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'main' seems OK

}

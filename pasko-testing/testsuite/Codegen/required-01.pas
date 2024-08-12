{
RUN: %pasko --mode=ir-dump %s | FileCheck %s
}

program test(output);
type
   my_enum = (red, blue, green);
   my_subrange = 1..100;

function my_abs_int(x: integer): integer;
begin
  my_abs_int := abs(x);
end;

function my_sqr_int(x: integer): integer;
begin
  my_sqr_int := sqr(x);
end;

function my_abs_real(x: real): real;
begin
  my_abs_real := abs(x);
end;

function my_sqr_real(x: real): real;
begin
  my_sqr_real := sqr(x);
end;

function my_sin(x: real): real;
begin
  my_sin := sin(x);
end;

function my_cos(x: real): real;
begin
  my_cos := cos(x);
end;

function my_exp(x: real): real;
begin
  my_exp := exp(x);
end;

function my_ln(x: real): real;
begin
  my_ln := ln(x);
end;

function my_sqrt(x: real): real;
begin
  my_sqrt := sqrt(x);
end;

function my_arctan(x: real): real;
begin
  my_arctan := arctan(x);
end;

function my_trunc(x: real) : integer;
begin
  my_trunc := trunc(x);
end;

function my_round(x: real) : integer;
begin
  my_round := round(x);
end;

function my_ord_i(x: integer): integer;
begin
  my_ord_i := ord(x);
end;

function my_ord_b(x: boolean): integer;
begin
  my_ord_b := ord(x);
end;

function my_ord_c(x: char): integer;
begin
  my_ord_c := ord(x);
end;

function my_ord_sub(x: my_subrange): integer;
begin
  my_ord_sub := ord(x);
end;

function my_ord_enum(x: my_enum): integer;
begin
  my_ord_enum := ord(x);
end;

function my_chr(x: integer): char;
begin
  my_chr := chr(x);
end;

function my_succ(x: my_subrange): my_subrange;
begin
  my_succ := succ(x);
end;

function my_pred(x: my_subrange): my_subrange;
begin
  my_pred := succ(x);
end;

begin
end.

{

CHECK: *** IR for function 'my_abs_int'
CHECK-NEXT: function u0:0(i64) -> i64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.i64 v2
CHECK-NEXT:     v4 = iabs v3
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     store v4, v5
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.i64 v6
CHECK-NEXT:     return v7
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_abs_int' seems OK

CHECK: *** IR for function 'my_sqr_int'
CHECK-NEXT: function u0:1(i64) -> i64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.i64 v2
CHECK-NEXT:     v4 = imul v3, v3
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     store v4, v5
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.i64 v6
CHECK-NEXT:     return v7
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_sqr_int' seems OK

CHECK: *** IR for function 'my_abs_real'
CHECK-NEXT: function u0:2(f64) -> f64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-EMPTY:
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.f64 v2
CHECK-NEXT:     v4 = fabs v3
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     store v4, v5
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.f64 v6
CHECK-NEXT:     return v7
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_abs_real' seems OK

CHECK: *** IR for function 'my_sqr_real'
CHECK-NEXT: function u0:3(f64) -> f64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-EMPTY:
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.f64 v2
CHECK-NEXT:     v4 = fmul v3, v3
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     store v4, v5
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.f64 v6
CHECK-NEXT:     return v7
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_sqr_real' seems OK

CHECK: *** IR for function 'my_sin'
CHECK-NEXT: function u0:4(f64) -> f64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-NEXT:     sig0 = (f64) -> f64 system_v
CHECK-NEXT:     fn0 = u0:5 sig0
CHECK-EMPTY:
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.f64 v2
CHECK-NEXT:     v4 = call fn0(v3)
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     store v4, v5
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.f64 v6
CHECK-NEXT:     return v7
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_sin' seems OK

CHECK: *** IR for function 'my_cos'
CHECK-NEXT: function u0:6(f64) -> f64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-NEXT:     sig0 = (f64) -> f64 system_v
CHECK-NEXT:     fn0 = u0:7 sig0
CHECK-EMPTY:
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.f64 v2
CHECK-NEXT:     v4 = call fn0(v3)
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     store v4, v5
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.f64 v6
CHECK-NEXT:     return v7
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_cos' seems OK

CHECK: *** IR for function 'my_exp'
CHECK-NEXT: function u0:8(f64) -> f64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-NEXT:     sig0 = (f64) -> f64 system_v
CHECK-NEXT:     fn0 = u0:9 sig0
CHECK-EMPTY:
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.f64 v2
CHECK-NEXT:     v4 = call fn0(v3)
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     store v4, v5
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.f64 v6
CHECK-NEXT:     return v7
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_exp' seems OK

CHECK: *** IR for function 'my_ln'
CHECK-NEXT: function u0:10(f64) -> f64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-NEXT:     sig0 = (f64) -> f64 system_v
CHECK-NEXT:     fn0 = u0:11 sig0
CHECK-EMPTY:
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.f64 v2
CHECK-NEXT:     v4 = call fn0(v3)
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     store v4, v5
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.f64 v6
CHECK-NEXT:     return v7
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_ln' seems OK

CHECK: *** IR for function 'my_sqrt'
CHECK-NEXT: function u0:12(f64) -> f64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-EMPTY:
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.f64 v2
CHECK-NEXT:     v4 = sqrt v3
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     store v4, v5
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.f64 v6
CHECK-NEXT:     return v7
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_sqrt' seems OK

CHECK: *** IR for function 'my_arctan'
CHECK-NEXT: function u0:13(f64) -> f64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-NEXT:     sig0 = (f64) -> f64 system_v
CHECK-NEXT:     fn0 = u0:14 sig0
CHECK-EMPTY:
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.f64 v2
CHECK-NEXT:     v4 = call fn0(v3)
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     store v4, v5
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.f64 v6
CHECK-NEXT:     return v7
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_arctan' seems OK

CHECK: *** IR for function 'my_trunc'
CHECK-NEXT: function u0:15(f64) -> i64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-EMPTY:
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.f64 v2
CHECK-NEXT:     v4 = trunc v3
CHECK-NEXT:     v5 = fcvt_to_sint.i64 v4
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     store v5, v6
CHECK-NEXT:     v7 = stack_addr.i64 ss0
CHECK-NEXT:     v8 = load.i64 v7
CHECK-NEXT:     return v8
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_trunc' seems OK

CHECK: *** IR for function 'my_round'
CHECK-NEXT: function u0:16(f64) -> i64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-EMPTY:
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.f64 v2
CHECK-NEXT:     v4 = nearest v3
CHECK-NEXT:     v5 = fcvt_to_sint.i64 v4
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     store v5, v6
CHECK-NEXT:     v7 = stack_addr.i64 ss0
CHECK-NEXT:     v8 = load.i64 v7
CHECK-NEXT:     return v8
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_round' seems OK

CHECK: *** IR for function 'my_ord_i'
CHECK-NEXT: function u0:17(i64) -> i64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.i64 v2
CHECK-NEXT:     v4 = stack_addr.i64 ss0
CHECK-NEXT:     store v3, v4
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     v6 = load.i64 v5
CHECK-NEXT:     return v6
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_ord_i' seems OK

CHECK: *** IR for function 'my_ord_b'
CHECK-NEXT: function u0:18(i8) -> i64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 1
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i8):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.i8 v2
CHECK-NEXT:     v4 = uextend.i64 v3
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     store v4, v5
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.i64 v6
CHECK-NEXT:     return v7
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_ord_b' seems OK

CHECK: *** IR for function 'my_ord_c'
CHECK-NEXT: function u0:19(i32) -> i64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 4
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i32):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.i32 v2
CHECK-NEXT:     v4 = uextend.i64 v3
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     store v4, v5
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.i64 v6
CHECK-NEXT:     return v7
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_ord_c' seems OK

CHECK: *** IR for function 'my_ord_sub'
CHECK-NEXT: function u0:20(i64) -> i64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.i64 v2
CHECK-NEXT:     v4 = stack_addr.i64 ss0
CHECK-NEXT:     store v3, v4
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     v6 = load.i64 v5
CHECK-NEXT:     return v6
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_ord_sub' seems OK

CHECK: *** IR for function 'my_ord_enum'
CHECK-NEXT: function u0:21(i64) -> i64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.i64 v2
CHECK-NEXT:     v4 = stack_addr.i64 ss0
CHECK-NEXT:     store v3, v4
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     v6 = load.i64 v5
CHECK-NEXT:     return v6
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_ord_enum' seems OK

CHECK: *** IR for function 'my_chr'
CHECK-NEXT: function u0:22(i64) -> i32 system_v {
CHECK-NEXT:     ss0 = explicit_slot 4
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.i64 v2
CHECK-NEXT:     v4 = ireduce.i32 v3
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     store v4, v5
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.i32 v6
CHECK-NEXT:     return v7
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_chr' seems OK

CHECK: *** IR for function 'my_succ'
CHECK-NEXT: function u0:23(i64) -> i64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.i64 v2
CHECK-NEXT:     v4 = iadd_imm v3, 1
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     store v4, v5
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.i64 v6
CHECK-NEXT:     return v7
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_succ' seems OK

CHECK: *** IR for function 'my_pred'
CHECK-NEXT: function u0:24(i64) -> i64 system_v {
CHECK-NEXT:     ss0 = explicit_slot 8
CHECK-NEXT:     ss1 = explicit_slot 8
CHECK-EMPTY:
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     v1 = stack_addr.i64 ss1
CHECK-NEXT:     store v0, v1
CHECK-NEXT:     v2 = stack_addr.i64 ss1
CHECK-NEXT:     v3 = load.i64 v2
CHECK-NEXT:     v4 = iadd_imm v3, 1
CHECK-NEXT:     v5 = stack_addr.i64 ss0
CHECK-NEXT:     store v4, v5
CHECK-NEXT:     v6 = stack_addr.i64 ss0
CHECK-NEXT:     v7 = load.i64 v6
CHECK-NEXT:     return v7
CHECK-NEXT: }
CHECK-NEXT: *** IR for function 'my_pred' seems OK

}

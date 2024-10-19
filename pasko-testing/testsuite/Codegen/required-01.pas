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

CHECK: *** IR for 'my_abs_int'
CHECK-NEXT: function u0:0(i64) -> i64 system_v {
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v1 = iabs v0
CHECK-NEXT:     ! my_abs_int ← v1 
CHECK-NEXT:     ! v1 → my_abs_int 
CHECK-NEXT:     return v1
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_abs_int' seems OK

CHECK: *** IR for 'my_sqr_int'
CHECK-NEXT: function u0:1(i64) -> i64 system_v {
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v1 = imul v0, v0
CHECK-NEXT:     ! my_sqr_int ← v1 
CHECK-NEXT:     ! v1 → my_sqr_int 
CHECK-NEXT:     return v1
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_sqr_int' seems OK

CHECK: *** IR for 'my_abs_real'
CHECK-NEXT: function u0:2(f64) -> f64 system_v {
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v1 = fabs v0
CHECK-NEXT:     ! my_abs_real ← v1 
CHECK-NEXT:     ! v1 → my_abs_real 
CHECK-NEXT:     return v1
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_abs_real' seems OK

CHECK: *** IR for 'my_sqr_real'
CHECK-NEXT: function u0:3(f64) -> f64 system_v {
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v1 = fmul v0, v0
CHECK-NEXT:     ! my_sqr_real ← v1 
CHECK-NEXT:     ! v1 → my_sqr_real 
CHECK-NEXT:     return v1
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_sqr_real' seems OK

CHECK: *** IR for 'my_sin'
CHECK-NEXT: function u0:4(f64) -> f64 system_v {
CHECK-NEXT:     sig0 = (f64) -> f64 system_v
CHECK-NEXT:     fn0 = u0:5 sig0 ; __pasko_sin_f64
CHECK-EMPTY: 
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v1 = call fn0(v0)
CHECK-NEXT:     ! my_sin ← v1 
CHECK-NEXT:     ! v1 → my_sin 
CHECK-NEXT:     return v1
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_sin' seems OK

CHECK: *** IR for 'my_cos'
CHECK-NEXT: function u0:6(f64) -> f64 system_v {
CHECK-NEXT:     sig0 = (f64) -> f64 system_v
CHECK-NEXT:     fn0 = u0:7 sig0 ; __pasko_cos_f64
CHECK-EMPTY:
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v1 = call fn0(v0)
CHECK-NEXT:     ! my_cos ← v1 
CHECK-NEXT:     ! v1 → my_cos 
CHECK-NEXT:     return v1
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_cos' seems OK

CHECK: *** IR for 'my_exp'
CHECK-NEXT: function u0:8(f64) -> f64 system_v {
CHECK-NEXT:     sig0 = (f64) -> f64 system_v
CHECK-NEXT:     fn0 = u0:9 sig0 ; __pasko_exp_f64
CHECK-EMPTY:
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v1 = call fn0(v0)
CHECK-NEXT:     ! my_exp ← v1 
CHECK-NEXT:     ! v1 → my_exp 
CHECK-NEXT:     return v1
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_exp' seems OK

CHECK: *** IR for 'my_ln'
CHECK-NEXT: function u0:10(f64) -> f64 system_v {
CHECK-NEXT:     sig0 = (f64) -> f64 system_v
CHECK-NEXT:     fn0 = u0:11 sig0 ; __pasko_ln_f64
CHECK-EMPTY:
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v1 = call fn0(v0)
CHECK-NEXT:     ! my_ln ← v1 
CHECK-NEXT:     ! v1 → my_ln 
CHECK-NEXT:     return v1
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_ln' seems OK

CHECK: *** IR for 'my_sqrt'
CHECK-NEXT: function u0:12(f64) -> f64 system_v {
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v1 = sqrt v0
CHECK-NEXT:     ! my_sqrt ← v1 
CHECK-NEXT:     ! v1 → my_sqrt 
CHECK-NEXT:     return v1
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_sqrt' seems OK

CHECK: *** IR for 'my_arctan'
CHECK-NEXT: function u0:13(f64) -> f64 system_v {
CHECK-NEXT:     sig0 = (f64) -> f64 system_v
CHECK-NEXT:     fn0 = u0:14 sig0 ; __pasko_arctan_f64
CHECK-EMPTY:
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v1 = call fn0(v0)
CHECK-NEXT:     ! my_arctan ← v1 
CHECK-NEXT:     ! v1 → my_arctan 
CHECK-NEXT:     return v1
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_arctan' seems OK

CHECK: *** IR for 'my_trunc'
CHECK-NEXT: function u0:15(f64) -> i64 system_v {
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v1 = trunc v0
CHECK-NEXT:     v2 = fcvt_to_sint.i64 v1
CHECK-NEXT:     ! my_trunc ← v2 
CHECK-NEXT:     ! v2 → my_trunc 
CHECK-NEXT:     return v2
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_trunc' seems OK

CHECK: *** IR for 'my_round'
CHECK-NEXT: function u0:16(f64) -> i64 system_v {
CHECK-NEXT: block0(v0: f64):
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v1 = nearest v0
CHECK-NEXT:     v2 = fcvt_to_sint.i64 v1
CHECK-NEXT:     ! my_round ← v2 
CHECK-NEXT:     ! v2 → my_round 
CHECK-NEXT:     return v2
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_round' seems OK

CHECK: *** IR for 'my_ord_i'
CHECK-NEXT: function u0:17(i64) -> i64 system_v {
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     ! v0 → my_ord_i 
CHECK-NEXT:     return v0
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_ord_i' seems OK

CHECK: *** IR for 'my_ord_b'
CHECK-NEXT: function u0:18(i8) -> i64 system_v {
CHECK-NEXT: block0(v0: i8):
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v1 = uextend.i64 v0
CHECK-NEXT:     ! my_ord_b ← v1 
CHECK-NEXT:     ! v1 → my_ord_b 
CHECK-NEXT:     return v1
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_ord_b' seems OK

CHECK: *** IR for 'my_ord_c'
CHECK-NEXT: function u0:19(i32) -> i64 system_v {
CHECK-NEXT: block0(v0: i32):
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v1 = uextend.i64 v0
CHECK-NEXT:     ! my_ord_c ← v1 
CHECK-NEXT:     ! v1 → my_ord_c 
CHECK-NEXT:     return v1
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_ord_c' seems OK

CHECK: *** IR for 'my_ord_sub'
CHECK-NEXT: function u0:20(i64) -> i64 system_v {
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     ! v0 → my_ord_sub 
CHECK-NEXT:     return v0
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_ord_sub' seems OK

CHECK: *** IR for 'my_ord_enum'
CHECK-NEXT: function u0:21(i64) -> i64 system_v {
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     ! v0 → my_ord_enum 
CHECK-NEXT:     return v0
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_ord_enum' seems OK

CHECK: *** IR for 'my_chr'
CHECK-NEXT: function u0:22(i64) -> i32 system_v {
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v1 = ireduce.i32 v0
CHECK-NEXT:     ! my_chr ← v1 
CHECK-NEXT:     ! v1 → my_chr 
CHECK-NEXT:     return v1
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_chr' seems OK

CHECK: *** IR for 'my_succ'
CHECK-NEXT: function u0:23(i64) -> i64 system_v {
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v1 = iadd_imm v0, 1
CHECK-NEXT:     ! my_succ ← v1 
CHECK-NEXT:     ! v1 → my_succ 
CHECK-NEXT:     return v1
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_succ' seems OK

CHECK: *** IR for 'my_pred'
CHECK-NEXT: function u0:24(i64) -> i64 system_v {
CHECK-NEXT: block0(v0: i64):
CHECK-NEXT:     ! v0 → x 
CHECK-NEXT:     v1 = iadd_imm v0, 1
CHECK-NEXT:     ! my_pred ← v1 
CHECK-NEXT:     ! v1 → my_pred 
CHECK-NEXT:     return v1
CHECK-NEXT: }
CHECK-NEXT: *** IR for 'my_pred' seems OK

}

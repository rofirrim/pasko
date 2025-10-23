use pasko_frontend::diagnostics;
use pasko_frontend::parser;
use pasko_frontend::span;

mod common;

use common::CheckDiagnostics;

fn parse_tree_success(input: &str) {
    let mut diags = diagnostics::Diagnostics::new();
    let p = parser::parse_pasko_program(input, &mut diags);

    assert!(p.is_some(), "no AST was returned");
    assert!(diags.num_diagnostics() == 0, "Diagnostics were emitted");
}

fn parse_tree_failure(input: &str) {
    let mut diags = diagnostics::Diagnostics::new();
    let p = parser::parse_pasko_program(input, &mut diags);

    assert!(p.is_none(), "some AST was returned?");
    assert!(diags.num_diagnostics() != 0, "No diagnostics were emitted?");
}

fn parse_tree_failure_check_diags(input: &str, errors: Vec<String>) {
    let linemap = span::LineMap::new(input, 4);
    let mut diags = diagnostics::Diagnostics::new();
    let p = parser::parse_pasko_program(input, &mut diags);

    assert!(p.is_none(), "some AST was returned?");
    assert!(diags.num_diagnostics() != 0, "No diagnostics were emitted?");

    let mut check_diags = CheckDiagnostics::new();
    errors.iter().for_each(|s| check_diags.check_error(s));

    diags.report(&check_diags, &linemap);
    assert_eq!(
        diags.num_diagnostics(),
        check_diags.num_diagnostics_seen(),
        "fewer diagnostics emitted than checked for"
    );
}

#[test]
fn basic_test() {
    parse_tree_success("program main; begin end.");
}

#[test]
fn fail_parsing() {
    parse_tree_failure("program program;");
}

#[test]
fn fail_parsing_diagnostic() {
    parse_tree_failure_check_diags("program main;", vec!["unexpected end of file".to_string()]);

    parse_tree_failure_check_diags(
        r#"
program test;
var
  x : integer;
begin
   x = 3;
end.
    "#,
        vec!["unexpected token =".to_string()],
    );

    parse_tree_failure_check_diags(
        "program program;",
        vec!["unexpected token program".to_string()],
    );
}

#[test]
fn success_mvp() {
    parse_tree_success(
        r#"
program test;
var
  a : integer;
begin
  readln(a);
  writeln('You entered: ', a);
end.
"#,
    );

    parse_tree_success(
        r#"
program test(input, output);
var
  x : integer;
begin
   x := 3;
end.
    "#,
    );

    parse_tree_success(
        r#"
program test;
var
  aa : real;
  bbb : integer;
  cccc : boolean;
begin
  aa := bbb + aa;
  aaa := bbb    + aa
    +
    cccc;
end."#,
    );

    parse_tree_success(
        r#"
program test(input, output);
var
  x : integer;
  y, z : real;
begin
   x := 3;
   y := 2.3;
   z := x + y;
end."#,
    );
}

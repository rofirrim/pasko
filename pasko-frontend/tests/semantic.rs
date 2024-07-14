use pasko_frontend::diagnostics;
use pasko_frontend::dump;
use pasko_frontend::parser;
use pasko_frontend::scope;
use pasko_frontend::semantic;
use pasko_frontend::visitor::Visitable;

mod common;

use common::CheckDiagnostics;

fn semantic_check_diags(input: &str, errors: Vec<String>) {
    let mut diags = diagnostics::Diagnostics::new();
    let mut p = parser::parse_pasko_program(input, &mut diags);

    let mut semantic_context = semantic::SemanticContext::new();
    let mut scope = scope::Scope::new();

    match p.as_mut() {
        Some(parse) => {
            semantic::check_program(parse, &mut semantic_context, &mut diags, &mut scope);
        }
        None => {
            panic!("no AST was created?");
        }
    }

    let mut check_diags = CheckDiagnostics::new();
    errors.iter().for_each(|s| check_diags.check_error(&s));

    diags.report(&check_diags);
    assert_eq!(
        errors.len(),
        check_diags.num_diagnostics_seen(),
        "fewer diagnostics emitted than checked for"
    );
}

fn do_ast_dump(input: &str) -> String {
    let mut diags = diagnostics::Diagnostics::new();
    let mut p = parser::parse_pasko_program(input, &mut diags);

    let mut semantic_context = semantic::SemanticContext::new();
    let mut scope = scope::Scope::new();

    match p.as_mut() {
        Some(parse) => {
            semantic::check_program(parse, &mut semantic_context, &mut diags, &mut scope);

            let mut dumper = dump::ASTDumper::new(&input, &semantic_context);
            dumper.set_no_ids();
            parse.get().walk_mut(&mut dumper, parse.loc(), parse.id());
            dumper.to_string()
        }
        None => {
            panic!("no AST was created?");
        }
    }
}

#[test]
fn diagnostic_not_implemented() {
    semantic_check_diags(
        r#"
program test(input, output);
var
  a : set of integer;
begin
  a := [1..10];
end.
  "#,
        vec!["sorry, range expressions not implemented yet".to_string()],
    );
}

#[test]
fn semantic_diagnostics() {
    semantic_check_diags(
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
end.
  "#,
        vec![
            "identifier 'aaa' not found in this scope".to_string(),
            "operator '+' cannot be applied to operands of type real and boolean".to_string(),
        ],
    )
}

#[test]
fn no_diagnostics() {
    semantic_check_diags(
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
        vec![],
    );
}

#[test]
fn ast_dump() {
    let s = do_ast_dump(
        r#"
program test(input, output);
var
  x : integer;
  y, z : real;
begin
   x := 3;
   y := 2.3;
   z := x + y
end.
"#
        .trim(),
    );

    println!("{}", s.trim());

    assert_eq!(
        s.trim(),
        r#"
Program 1:1 
├─╴ProgramHeading 1:1 "test" ["input", "output"]
└─╴ProgramBlock 2:1 
   └─╴Block 2:1 
      ├─╴VariableDeclarationPart 2:1 
      │  ├─╴VariableDeclaration 3:3 ["x"]
      │  │  └─╴TypeIdentifier 3:7 "integer" integer
      │  └─╴VariableDeclaration 4:3 ["y", "z"]
      │     └─╴TypeIdentifier 4:10 "real" real
      └─╴Statement 5:1 
         └─╴StmtCompound 5:1 
            ├─╴StmtAssignment 6:4 lhs integer rhs integer
            │  ├─╴AssigVariable 6:4 "x" integer
            │  └─╴ExprConst 6:9 "integer" <<no const>>
            │     └─╴ConstInteger 6:9 3 integer
            ├─╴StmtAssignment 7:4 lhs real rhs real
            │  ├─╴AssigVariable 7:4 "y" real
            │  └─╴ExprConst 7:9 "real" <<no const>>
            │     └─╴ConstReal 7:9 2.3 real
            └─╴StmtAssignment 8:4 lhs real rhs real
               ├─╴AssigVariable 8:4 "z" real
               └─╴BinOp 8:9 + real
                  ├─╴Conversion 8:9 real
                  │  └─╴ExprVariable 8:9 integer
                  │     └─╴AssigVariable 8:9 "x" integer
                  └─╴ExprVariable 8:13 real
                     └─╴AssigVariable 8:13 "y" real
"#
        .trim()
    );
}

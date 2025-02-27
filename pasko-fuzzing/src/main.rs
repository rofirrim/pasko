#[macro_use]
extern crate afl;

use pasko_frontend;

fn main() {
    fuzz!(|data: &[u8]| {
        if let Ok(input) = std::str::from_utf8(data) {
            let mut diagnostics = pasko_frontend::diagnostics::Diagnostics::new();
            let parse_result =
                pasko_frontend::parser::parse_pasko_program(&input, &mut diagnostics);
            if let Some(mut program) = parse_result {
                let mut semantic_context = pasko_frontend::semantic::SemanticContext::new(&input);
                pasko_frontend::semantic::check_program(
                    &mut program,
                    &mut semantic_context,
                    &mut diagnostics,
                );
            } else {
                // Report any parsing error and finish.
                assert!(
                    diagnostics.num_error() > 0,
                    "if the parse fails we must diagnose an error"
                );
            }
        }
    });
}

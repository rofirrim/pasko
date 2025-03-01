use lalrpop_util::lalrpop_mod;
use lalrpop_util::ParseError;

use crate::ast;
use crate::diagnostics;
use crate::lexer;
use crate::span;

lalrpop_mod!(pasko); // synthesized by LALRPOP

pub fn parse_pasko_program(
    input: &str,
    diagnostics: &mut diagnostics::Diagnostics,
) -> Option<span::SpannedBox<ast::Program>> {
    let lex = lexer::Lexer::new(input);

    let r = pasko::ProgramParser::new().parse(lex);
    if let Err(x) = &r {
        match x {
            ParseError::InvalidToken { location } => {
                let locus = span::SpanLoc(*location, *location);
                diagnostics.add(
                    diagnostics::DiagnosticKind::Error,
                    locus,
                    "unexpected token".to_string(),
                );
            }
            ParseError::UnrecognizedEof { location, expected } => {
                let locus = span::SpanLoc(*location, *location);
                let message = format!(
                    "unexpected end of file, expecting one of {}",
                    expected.join(", ")
                );
                diagnostics.add(diagnostics::DiagnosticKind::Error, locus, message);
            }
            ParseError::UnrecognizedToken { token, expected } => {
                let locus = span::SpanLoc(token.0, token.2);
                let message = format!(
                    "unexpected token {}, expecting one of {}",
                    token.1,
                    expected.join(", ")
                );
                diagnostics.add(diagnostics::DiagnosticKind::Error, locus, message);
            }
            ParseError::ExtraToken { token } => {
                let locus = span::SpanLoc(token.0, token.2);
                let message = format!("extra token {}", token.1);
                diagnostics.add(diagnostics::DiagnosticKind::Error, locus, message);
            }
            ParseError::User {
                error:
                    lexer::LexicalError {
                        start,
                        end,
                        message,
                    },
            } => {
                // Why don't we have locus here?
                let locus = span::SpanLoc(*start, *end);
                diagnostics.add(
                    diagnostics::DiagnosticKind::Error,
                    locus,
                    message.to_string(),
                );
            }
        }
    }
    r.ok()
}

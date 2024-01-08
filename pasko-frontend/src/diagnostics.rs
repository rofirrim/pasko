use crate::span::SpanLoc;

#[derive(Debug, Eq, PartialEq)]
pub enum DiagnosticKind {
    Info,
    Warning,
    Error,
}

pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub locus: SpanLoc,
    pub message: String,
    pub extra_locus: Option<Vec<SpanLoc>>,
    pub extra_diagnostics: Option<Vec<Diagnostic>>,
}

impl Diagnostic {
    pub fn new(kind: DiagnosticKind, locus: SpanLoc, message: String) -> Diagnostic {
        Diagnostic {
            kind,
            locus,
            message,
            extra_locus: None,
            extra_diagnostics: None,
        }
    }

    pub fn new_with_extra(
        kind: DiagnosticKind,
        locus: SpanLoc,
        message: String,
        extra_locus: Vec<SpanLoc>,
        extra_diagnostics: Vec<Diagnostic>,
    ) -> Diagnostic {
        Diagnostic {
            kind,
            locus,
            message,
            extra_locus: Some(extra_locus),
            extra_diagnostics: Some(extra_diagnostics),
        }
    }
}

pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
    num_error: usize,
}

impl Diagnostics {
    pub fn new() -> Diagnostics {
        Diagnostics {
            diagnostics: vec![],
            num_error: 0,
        }
    }
    pub fn add(&mut self, kind: DiagnosticKind, locus: SpanLoc, message: String) {
        match kind {
            DiagnosticKind::Error => {
                self.num_error += 1;
            }
            _ => {}
        }
        self.diagnostics.push(Diagnostic::new(kind, locus, message));
    }

    pub fn add_with_extra(
        &mut self,
        kind: DiagnosticKind,
        locus: SpanLoc,
        message: String,
        extra_locus: Vec<SpanLoc>,
        extra_diagnostics: Vec<Diagnostic>,
    ) {
        match kind {
            DiagnosticKind::Error => {
                self.num_error += 1;
            }
            _ => {}
        }
        self.diagnostics.push(Diagnostic::new_with_extra(
            kind,
            locus,
            message,
            extra_locus,
            extra_diagnostics,
        ));
    }

    // FIXME: We could do something smarter here, like sorting by line.
    pub fn report(&self, emitter: &dyn DiagnosticEmitter) {
        self.diagnostics.iter().for_each(|d| emitter.emit(d));
    }

    pub fn num_diagnostics(&self) -> usize {
        self.diagnostics.len()
    }

    pub fn num_error(&self) -> usize {
        self.num_error
    }
}

pub trait DiagnosticEmitter {
    fn emit(&self, diag: &Diagnostic);
}

use std::convert::From;
use std::process::ExitCode;

impl From<Diagnostics> for ExitCode {
    fn from(diags: Diagnostics) -> ExitCode {
        if diags.num_error() == 0 {
            ExitCode::SUCCESS
        } else {
            ExitCode::FAILURE
        }
    }
}

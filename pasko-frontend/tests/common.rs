use pasko_frontend::diagnostics;
use std::cell::Cell;

pub struct CheckDiagnostics {
    checks: Vec<(diagnostics::DiagnosticKind, String)>,
    idx: Cell<usize>,
}

impl CheckDiagnostics {
    pub fn new() -> CheckDiagnostics {
        CheckDiagnostics {
            checks: vec![],
            idx: Cell::new(0),
        }
    }

    pub fn check_error(&mut self, s: &str) {
        self.checks
            .push((diagnostics::DiagnosticKind::Error, s.to_string()));
    }

    pub fn num_diagnostics_seen(&self) -> usize {
        self.idx.get()
    }
}

impl diagnostics::DiagnosticEmitter for CheckDiagnostics {
    fn emit(&self, diag: &diagnostics::Diagnostic) {
        let current_idx = self.idx.get();
        self.idx.set(current_idx + 1);

        assert!(
            current_idx < self.checks.len(),
            "more diagnostics emitted than checked"
        );
        assert_eq!(
            self.checks[current_idx].0, diag.kind,
            "unexpected kind of diagnostic"
        );
        assert!(
            diag.message.starts_with(&self.checks[current_idx].1),
            "diagnostic message {:?} does not start with {:?}",
            diag.message,
            self.checks[current_idx].1
        );
    }
}

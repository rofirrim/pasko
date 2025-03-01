use pasko_frontend::{self, span};

pub struct SimpleEmitter<'input_file> {
    filename: &'input_file str,
    input: &'input_file str,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct LineAndCol {
    line: usize,
    col: usize,
    is_main: bool,
}

impl LineAndCol {
    fn new(linemap: &span::LineMap, offset: usize, is_main: bool) -> LineAndCol {
        let (line, col) = linemap.offset_to_line_and_col(offset);

        LineAndCol { line, col, is_main }
    }
}

impl<'input_file> SimpleEmitter<'input_file> {
    pub fn new(filename: &'input_file str, input: &'input_file str) -> SimpleEmitter<'input_file> {
        SimpleEmitter { filename, input }
    }

    fn get_line_and_col(
        &self,
        linemap: &span::LineMap,
        location: &span::SpanLoc,
        all_line_locations: &mut Vec<(LineAndCol, LineAndCol)>,
        is_main: bool,
    ) {
        all_line_locations.push((
            LineAndCol::new(linemap, location.0, is_main),
            LineAndCol::new(linemap, location.1, is_main),
        ));
    }

    fn print_location(
        &self,
        linemap: &span::LineMap,
        main_location: span::SpanLoc,
        extra_locations: Vec<span::SpanLoc>,
    ) {
        let mut all_locations = extra_locations.clone();
        all_locations.push(main_location);

        // Now, sort locations with starting offset because they should not overlap in the diagnostics.
        all_locations.sort_by(|a, b| a.0.cmp(&b.0));

        let mut all_line_locations: Vec<(LineAndCol, LineAndCol)> = Vec::new();

        for location in all_locations.iter() {
            self.get_line_and_col(
                linemap,
                location,
                &mut all_line_locations,
                main_location == *location,
            );
        }

        let first_line = all_line_locations.iter().map(|l| l.0.line).min().unwrap();
        let last_line = all_line_locations.iter().map(|l| l.1.line).max().unwrap();

        eprintln!();
        for current_line in first_line..=last_line {
            // println!("Current line = {current_line}");
            let start_offset = linemap.start_of_line_offset(current_line).unwrap();
            let end_offset = linemap.end_of_line_offset(current_line).unwrap();
            // println!("start_offset = {start_offset} | end_offset = {end_offset}");

            let linenum_str = format!("{current_line:5} ");
            eprintln!("{linenum_str}│ {}", &self.input[start_offset..=end_offset]);

            let active_locations: Vec<_> = all_line_locations
                .iter()
                .filter(|e| e.0.line <= current_line && current_line <= e.1.line)
                .collect();

            // This is an empty line. Skip.
            if end_offset < start_offset {
                continue;
            }
            // Active locations should not overlap, so there can be more than one active per a single line.
            let max_col = end_offset - start_offset + 1;
            let mut carets: Vec<char> = (1..=max_col).map(|_| ' ').collect();

            if !active_locations.is_empty() {
                for active_location in active_locations {
                    // println!("{active_location:?} || {max_col}");
                    // Sometimes our column points to the newline which is not a character
                    // in the current line and would cause an out of bounds access.
                    // So use end_col instead of active_location.1.col
                    if active_location.0.line < current_line
                        && current_line < active_location.1.line
                    {
                        // This means that this span started in an earlier line and ends in a later line
                        // Fill all the caret line with a marker.
                        carets[0] = '╴';
                        for col in 2..max_col {
                            carets[col - 1] = '─';
                        }
                        carets[max_col - 1] = '╶';
                        // No overlaps!
                        break;
                    }
                    if active_location.0.line == current_line {
                        // We start in this line
                        if active_location.1.line == current_line {
                            // And we end in this line
                            if active_location.0.col + 1 == active_location.1.col {
                                // But we only take one column!
                                carets[active_location.0.col - 1] = '↑';
                            } else {
                                // We take more than one column.
                                carets[active_location.0.col - 1] = '╰';
                                let last_col = active_location.1.col - 1;
                                for col in (active_location.0.col + 1)..last_col {
                                    carets[col - 1] = '─';
                                }
                                carets[last_col - 1] = '╯';
                            }
                        } else {
                            // We don't end in this line, so fill it up until the end.
                            carets[active_location.0.col - 1] = '╰';
                            for col in active_location.0.col + 1..max_col {
                                carets[col - 1] = '─';
                            }
                            carets[max_col - 1] = '╶';
                        }
                    } else if active_location.1.line == current_line {
                        // We end in this line but we didn't start in this one either.
                        let end_col = std::cmp::min(max_col, active_location.1.col);
                        carets[0] = '╴';
                        for col in 2..end_col {
                            carets[col - 1] = '─';
                        }
                        carets[end_col - 1] = '╯';
                    }
                }

                let linenum_indent: String = (0..linenum_str.len()).map(|_| ' ').collect();
                let s: String = carets.into_iter().collect();
                let s = s.trim_end();
                eprintln!("{linenum_indent}│ {}", s);
            }
        }
        eprintln!();
    }
}

impl<'input_file> pasko_frontend::diagnostics::DiagnosticEmitter for SimpleEmitter<'input_file> {
    fn emit(&self, diag: &pasko_frontend::diagnostics::Diagnostic) {
        let diag_kind = match diag.kind {
            pasko_frontend::diagnostics::DiagnosticKind::Error => "error",
            pasko_frontend::diagnostics::DiagnosticKind::Warning => "warning",
            pasko_frontend::diagnostics::DiagnosticKind::Info => "info",
        };

        // Now compute a map between lines and offsets.
        let linemap = span::LineMap::new(self.input);

        let message = &diag.message;

        let main_location = diag.locus;
        eprintln!(
            "{}:{}:{}: {diag_kind}: {message}",
            self.filename,
            linemap.offset_to_line(main_location.0),
            linemap.offset_to_column(main_location.0)
        );

        // FIXME: This is odd.
        let extra_locations: Vec<_> = match &diag.extra_locus {
            None => vec![],
            Some(v) => v.clone(),
        };

        self.print_location(&linemap, main_location, extra_locations);

        // Emit extras if any.

        if let Some(extras) = &diag.extra_diagnostics {
            for d in extras.iter() {
                self.emit(d);
            }
        }
    }
}

use crate::utils;

// Infrastructure to support locations and identifiers.

pub trait Span {
    fn begin(&self) -> usize;
    fn end(&self) -> usize;
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct SpanLoc(pub usize, pub usize);

impl SpanLoc {
    pub fn new(start: usize, end: usize) -> SpanLoc {
        SpanLoc(start, end)
    }
    pub fn begin(&self) -> usize {
        self.0
    }
    pub fn end(&self) -> usize {
        self.1
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
pub struct SpanId(utils::Identifier);

impl SpanId {
    // Use this only for debugging purposes.
    pub fn get_number(&self) -> usize {
        self.0.get_number()
    }
}

impl From<SpanId> for utils::Identifier {
    fn from(s: SpanId) -> utils::Identifier {
        s.0
    }
}

#[derive(Debug)]
pub struct Spanned<T> {
    span_id: SpanId,
    loc: SpanLoc,
    child: T,
}

impl<T> Spanned<T> {
    pub fn new(loc: SpanLoc, child: T) -> Spanned<T> {
        Self {
            span_id: SpanId(utils::new_id()),
            loc,
            child,
        }
    }
    pub fn get(&self) -> &T {
        &self.child
    }
    pub fn get_mut(&mut self) -> &mut T {
        &mut self.child
    }
    pub fn loc(&self) -> &SpanLoc {
        &self.loc
    }
    pub fn id(&self) -> SpanId {
        self.span_id
    }
}

impl<T> Span for Spanned<T> {
    fn begin(&self) -> usize {
        self.loc.begin()
    }
    fn end(&self) -> usize {
        self.loc.end()
    }
}

#[derive(Debug)]
pub struct SpannedBox<T> {
    child: Option<Box<Spanned<T>>>,
}

impl<T> SpannedBox<T> {
    pub fn new(loc: SpanLoc, child: T) -> SpannedBox<T> {
        Self {
            child: Some(Box::new(Spanned::new(loc, child))),
        }
    }
    pub fn get(&self) -> &T {
        self.child.as_ref().unwrap().get()
    }

    pub fn get_mut(&mut self) -> &mut T {
        self.child.as_mut().unwrap().get_mut()
    }

    pub fn loc(&self) -> &SpanLoc {
        &self.child.as_ref().unwrap().loc
    }

    pub fn id(&self) -> SpanId {
        self.child.as_ref().unwrap().id()
    }

    // Mutating operations available to AST-mutating visitors.
    pub fn take(&mut self) -> Spanned<T> {
        assert!(self.child.is_some());
        let t = std::mem::take(&mut self.child).unwrap();
        *t
    }

    pub fn reset(&mut self, v: Spanned<T>) {
        assert!(self.child.is_none());
        self.child = Some(Box::new(v));
    }
}

impl<T> From<Spanned<T>> for SpannedBox<T> {
    fn from(v: Spanned<T>) -> SpannedBox<T> {
        Self {
            child: Some(Box::new(v)),
        }
    }
}

/// Convenient macro to create SpanLoc.
macro_rules! span_loc {
    ($start:expr, $end:expr) => {
        SpanLoc::new($start, $end)
    };
}

#[derive(Debug)]
pub struct LineMap {
    line_start: Vec<usize>,
    line_end: Vec<usize>,
    tab_size: usize,
    tab_locations: Vec<usize>,
}

impl LineMap {
    pub fn new(input: &str, tab_size: usize) -> LineMap {
        let mut result = LineMap {
            line_start: vec![],
            line_end: vec![],
            tab_size,
            tab_locations: vec![],
        };

        let mut prev_was_new_line = true;
        for (offset, c) in input.bytes().enumerate() {
            if prev_was_new_line {
                result.line_start.push(offset);
                prev_was_new_line = false;
            }
            if c as char == '\n' {
                result.line_end.push(offset);
                prev_was_new_line = true;
            } else if c as char == '\t' {
                result.tab_locations.push(offset);
            }
        }

        // The file ends without a newline, make an end.
        if !prev_was_new_line {
            result.line_end.push(input.len());
        }

        // println!("{result:?}");

        result
    }

    pub fn start_of_line_offset(&self, line: usize) -> Option<usize> {
        assert!(line > 0);
        if line <= self.line_start.len() {
            Some(self.line_start[line - 1])
        } else {
            None
        }
    }

    pub fn end_of_line_offset(&self, line: usize) -> Option<usize> {
        assert!(line > 0);
        if line <= self.line_end.len() {
            Some(self.line_end[line - 1] - 1)
        } else {
            None
        }
    }

    pub fn offset_to_line(&self, offset: usize) -> usize {
        self.offset_to_line_and_col(offset).0
    }

    pub fn offset_to_column(&self, offset: usize) -> usize {
        self.offset_to_line_and_col(offset).1
    }

    pub fn offset_to_line_and_col(&self, offset: usize) -> (usize, usize) {
        // println!("offset = {offset} | lines = {:?}", self.line_start);
        let line = self.line_start.partition_point(|x| *x <= offset);

        let mut next_tab = self
            .tab_locations
            .partition_point(|y| *y < self.line_start[line - 1]);

        let mut column = offset - self.line_start[line - 1] + 1;
        while next_tab < self.tab_locations.len() && self.tab_locations[next_tab] <= offset {
            column += self.tab_size - 1;
            next_tab += 1;
        }

        // dbg!(column);

        // (line, offset - self.line_start[line - 1] + 1)
        // (line, self.column_mapping[offset])
        (line, column)
    }
}

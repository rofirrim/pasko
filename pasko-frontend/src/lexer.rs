pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Clone, Debug)]
pub enum Tok {
    // Punctuation
    Assign,
    Colon,
    Comma,
    Deref,
    Different,
    Div,
    Dot,
    Ellipsis,
    Equal,
    GreaterOrEqualThan,
    GreaterThan,
    LeftParen,
    LeftSquareBracket,
    LowerOrEqualThan,
    LowerThan,
    Minus,
    Mul,
    Plus,
    RightParen,
    RightSquareBracket,
    Semicolon,
    // Compoound tokens
    Identifier(String),
    IntegerLiteral(String),
    RealLiteral(String),
    StringLiteral(String),
    // Keywords
    And,
    Array,
    Begin,
    Case,
    Const,
    IntegerDiv,
    Do,
    DownTo,
    Else,
    End,
    File,
    For,
    Forward,
    Function,
    Goto,
    If,
    In,
    Label,
    Mod,
    Nil,
    Not,
    Of,
    Or,
    Packed,
    Procedure,
    Program,
    Record,
    Remainder,
    Repeat,
    Set,
    Then,
    To,
    Type,
    Until,
    Var,
    While,
    With,
}

use std::fmt;

impl fmt::Display for Tok {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Tok::Assign => ":=",
                Tok::Colon => ":",
                Tok::Comma => ",",
                Tok::Deref => "^",
                Tok::Different => "<>",
                Tok::Div => "/",
                Tok::Dot => ".",
                Tok::Ellipsis => "..",
                Tok::Equal => "=",
                Tok::GreaterOrEqualThan => ">=",
                Tok::GreaterThan => ">",
                Tok::LeftParen => "(",
                Tok::LeftSquareBracket => "[",
                Tok::LowerOrEqualThan => "<=",
                Tok::LowerThan => "<",
                Tok::Minus => "-",
                Tok::Mul => "*",
                Tok::Plus => "+",
                Tok::RightParen => ")",
                Tok::RightSquareBracket => "]",
                Tok::Semicolon => ";",
                // Compoound tokens
                Tok::Identifier(s) => s.as_str(),
                Tok::IntegerLiteral(s) => s.as_str(),
                Tok::RealLiteral(s) => s.as_str(),
                Tok::StringLiteral(s) => s.as_str(),
                // Keywords
                Tok::And => "and",
                Tok::Array => "array",
                Tok::Begin => "begin",
                Tok::Case => "case",
                Tok::Const => "const",
                Tok::IntegerDiv => "div",
                Tok::Do => "do",
                Tok::DownTo => "downto",
                Tok::Else => "else",
                Tok::End => "end",
                Tok::File => "file",
                Tok::For => "for",
                Tok::Forward => "forward",
                Tok::Function => "function",
                Tok::Goto => "goto",
                Tok::If => "if",
                Tok::In => "in",
                Tok::Label => "label",
                Tok::Mod => "mod",
                Tok::Nil => "nil",
                Tok::Not => "not",
                Tok::Of => "of",
                Tok::Or => "or",
                Tok::Packed => "packed",
                Tok::Procedure => "procedure",
                Tok::Program => "program",
                Tok::Record => "record",
                Tok::Remainder => "rem",
                Tok::Repeat => "repeat",
                Tok::Set => "set",
                Tok::Then => "then",
                Tok::To => "to",
                Tok::Type => "type",
                Tok::Until => "until",
                Tok::Var => "var",
                Tok::While => "while",
                Tok::With => "with",
            }
        )
    }
}

#[derive(Debug)]
pub struct LexicalError {
    pub start: usize,
    pub end: usize,
    pub message: String,
}

use std::str::CharIndices;

pub struct Lexer<'input> {
    chars: CharIndices<'input>,
    peek_buffer: Vec<Option<(usize, char)>>,
    head: usize,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            chars: input.char_indices(),
            peek_buffer: vec![],
            head: 0,
        }
    }

    fn skip(&mut self) {
        if self.head == self.peek_buffer.len() {
            self.peek_buffer.clear();
            self.head = 0;
        } else {
            self.head += 1;
        }
    }

    fn peek_nth(&mut self, offset: usize) -> Option<(usize, char)> {
        while self.head + offset >= self.peek_buffer.len() {
            self.peek_buffer.push(self.chars.next());
        }
        self.peek_buffer[self.head + offset]
    }

    fn peek(&mut self) -> Option<(usize, char)> {
        self.peek_nth(0)
    }

    fn consume_comment(&mut self, offset: usize) -> (usize, usize) {
        let mut offset_end = offset + 1;
        let mut nesting_level = 1;
        while let Some((_, c2)) = self.peek() {
            self.skip();
            offset_end += 1;
            match c2 {
                '}' => {
                    nesting_level -= 1;
                    if nesting_level == 0 {
                        break;
                    }
                }
                '{' => {
                    nesting_level += 1;
                }
                // Old style comments.
                '(' => {
                    if let Some((_, '*')) = self.peek() {
                        self.skip();
                        nesting_level += 1;
                    }
                }
                '*' => {
                    if let Some((_, ')')) = self.peek() {
                        self.skip();
                        nesting_level -= 1;
                        if nesting_level == 0 {
                            break;
                        }
                    }
                }
                _ => {}
            }
        }

        (offset_end, nesting_level)
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Tok, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let s = self.peek();
            self.skip();
            s?;
            let (offset, c) = s.unwrap();

            match c {
                '\n' | '\r' | '\t' | ' ' => {
                    // Whitespace. Discard.
                }
                '{' => {
                    let (offset_end, nesting_level) = self.consume_comment(offset);
                    if nesting_level != 0 {
                        return Some(Err(LexicalError {
                            start: offset,
                            end: offset_end,
                            message: "unterminated comment".to_string(),
                        }));
                    }
                }
                ':' => {
                    let s2 = self.peek();
                    if let Some((_, c2)) = s2 {
                        if c2 == '=' {
                            self.skip();
                            return Some(Ok((offset, Tok::Assign, offset + 2)));
                        }
                    }
                    return Some(Ok((offset, Tok::Colon, offset + 1)));
                }
                ',' => return Some(Ok((offset, Tok::Comma, offset + 1))),
                '^' => return Some(Ok((offset, Tok::Deref, offset + 1))),
                '@' => return Some(Ok((offset, Tok::Deref, offset + 1))),
                '<' => {
                    let s2 = self.peek();
                    if let Some((_, c2)) = s2 {
                        if c2 == '=' {
                            self.skip();
                            return Some(Ok((offset, Tok::LowerOrEqualThan, offset + 2)));
                        } else if c2 == '>' {
                            self.skip();
                            return Some(Ok((offset, Tok::Different, offset + 2)));
                        }
                    }
                    return Some(Ok((offset, Tok::LowerThan, offset + 1)));
                }
                '/' => return Some(Ok((offset, Tok::Div, offset + 1))),
                '.' => {
                    let s2 = self.peek();
                    if let Some((_, c2)) = s2 {
                        if c2 == '.' {
                            self.skip();
                            return Some(Ok((offset, Tok::Ellipsis, offset + 2)));
                        } else if c2 == ')' {
                            // .) is ]
                            self.skip();
                            return Some(Ok((offset, Tok::RightSquareBracket, offset + 2)));
                        }
                    }
                    return Some(Ok((offset, Tok::Dot, offset + 1)));
                }
                '=' => return Some(Ok((offset, Tok::Equal, offset + 1))),
                '>' => {
                    let s2 = self.peek();
                    if let Some((_, c2)) = s2 {
                        if c2 == '=' {
                            self.skip();
                            return Some(Ok((offset, Tok::GreaterOrEqualThan, offset + 2)));
                        }
                    }
                    return Some(Ok((offset, Tok::GreaterThan, offset + 1)));
                }
                '(' => {
                    let s2 = self.peek();
                    if let Some((_, c2)) = s2 {
                        if c2 == '*' {
                            // (* starts an old style comment.
                            // Consume '*'
                            self.skip();
                            let (offset_end, nesting_level) = self.consume_comment(offset);
                            if nesting_level != 0 {
                                return Some(Err(LexicalError {
                                    start: offset,
                                    end: offset_end,
                                    message: "unterminated comment".to_string(),
                                }));
                            }
                            // Nothing else to do at this point.
                            continue;
                        } else if c2 == '.' {
                            // (. is [
                            // Consume '.'
                            self.skip();
                            return Some(Ok((offset, Tok::LeftSquareBracket, offset + 2)));
                        }
                    }
                    return Some(Ok((offset, Tok::LeftParen, offset + 1)));
                }
                '[' => return Some(Ok((offset, Tok::LeftSquareBracket, offset + 1))),
                '-' => return Some(Ok((offset, Tok::Minus, offset + 1))),
                '*' => return Some(Ok((offset, Tok::Mul, offset + 1))),
                '+' => return Some(Ok((offset, Tok::Plus, offset + 1))),
                ')' => return Some(Ok((offset, Tok::RightParen, offset + 1))),
                ']' => return Some(Ok((offset, Tok::RightSquareBracket, offset + 1))),
                ';' => return Some(Ok((offset, Tok::Semicolon, offset + 1))),
                '0'..='9' => {
                    let mut offset_end = offset + 1;
                    let mut tmp = c.to_string();
                    let mut is_real = false;
                    let mut is_real_with_fraction = true;
                    while let Some((_, c2)) = self.peek() {
                        match c2 {
                            '0'..='9' => {
                                tmp.push(c2);
                                offset_end += 1;
                                self.skip();
                            }
                            '.' => {
                                let is_followed_by_number = if let Some((_, c3)) = self.peek_nth(1)
                                {
                                    // 42.1
                                    // 42.. is not a valid real
                                    // 42.) would not be a valid real either an in fact it must be lexed as 42]
                                    c3.is_ascii_digit()
                                } else {
                                    false
                                };
                                if is_followed_by_number {
                                    is_real = true;
                                    tmp.push(c2);
                                    offset_end += 1;
                                    self.skip();
                                }
                                break;
                            }
                            'e' | 'E' => {
                                is_real = true;
                                is_real_with_fraction = false;
                                // No skip here!
                                break;
                            }
                            _ => {
                                break;
                            }
                        }
                    }

                    if is_real {
                        if is_real_with_fraction {
                            while let Some((_, c2)) = self.peek() {
                                match c2 {
                                    '0'..='9' => {
                                        tmp.push(c2);
                                        offset_end += 1;
                                        self.skip();
                                    }
                                    _ => {
                                        break;
                                    }
                                }
                            }
                        }
                        if let Some((_, c2)) = self.peek() {
                            if c2 == 'e' || c2 == 'E' {
                                tmp.push('e');
                                offset_end += 1;
                                self.skip();
                                if let Some((_, c3)) = self.peek() {
                                    if c3 == '+' || c3 == '-' {
                                        tmp.push(c3);
                                        offset_end += 1;
                                        self.skip();
                                    }
                                }
                                while let Some((_, c3)) = self.peek() {
                                    match c3 {
                                        '0'..='9' => {
                                            tmp.push(c3);
                                            offset_end += 1;
                                            self.skip();
                                        }
                                        _ => {
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if !is_real {
                        return Some(Ok((offset, Tok::IntegerLiteral(tmp), offset_end)));
                    } else {
                        return Some(Ok((offset, Tok::RealLiteral(tmp), offset_end)));
                    }
                }
                '\'' => {
                    let mut offset_end = offset + 1;
                    let mut tmp = String::new();
                    let mut inside_string = true;
                    while let Some((_, c2)) = self.peek() {
                        match c2 {
                            '\'' => {
                                offset_end += 1;
                                self.skip();
                                if let Some((_, c3)) = self.peek() {
                                    if c3 == '\'' {
                                        offset_end += 1;
                                        self.skip();
                                        tmp.push(c3);
                                    } else {
                                        inside_string = false;
                                        break;
                                    }
                                } else {
                                    inside_string = false;
                                    break;
                                }
                            }
                            '\r' | '\n' => {
                                break;
                            }
                            _ => {
                                offset_end += 1;
                                self.skip();
                                tmp.push(c2);
                            }
                        }
                    }
                    if inside_string {
                        return Some(Err(LexicalError {
                            start: offset,
                            end: offset_end,
                            message: "unterminated string literal".to_string(),
                        }));
                    }
                    if tmp.is_empty() {
                        return Some(Err(LexicalError {
                            start: offset,
                            end: offset_end,
                            message: "a string literal must contain at least one character"
                                .to_string(),
                        }));
                    }
                    return Some(Ok((offset, Tok::StringLiteral(tmp), offset_end)));
                }
                'a'..='z' | 'A'..='Z' => {
                    let mut tmp = c.to_string();
                    while let Some((_, c2)) = self.peek() {
                        match c2 {
                            '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {
                                tmp.push(c2);
                                self.skip();
                            }
                            _ => {
                                break;
                            }
                        }
                    }

                    // Now distinguish between a keyword or a general identifier.
                    let lowercase = tmp.to_lowercase();
                    let token = match lowercase.as_str() {
                        "and" => Tok::And,
                        "array" => Tok::Array,
                        "begin" => Tok::Begin,
                        "case" => Tok::Case,
                        "const" => Tok::Const,
                        "div" => Tok::IntegerDiv,
                        "do" => Tok::Do,
                        "downto" => Tok::DownTo,
                        "else" => Tok::Else,
                        "end" => Tok::End,
                        "file" => Tok::File,
                        "for" => Tok::For,
                        "forward" => Tok::Forward,
                        "function" => Tok::Function,
                        "goto" => Tok::Goto,
                        "if" => Tok::If,
                        "in" => Tok::In,
                        "label" => Tok::Label,
                        "mod" => Tok::Mod,
                        "nil" => Tok::Nil,
                        "not" => Tok::Not,
                        "of" => Tok::Of,
                        "or" => Tok::Or,
                        "packed" => Tok::Packed,
                        "procedure" => Tok::Procedure,
                        "program" => Tok::Program,
                        "record" => Tok::Record,
                        "rem" => Tok::Remainder,
                        "repeat" => Tok::Repeat,
                        "set" => Tok::Set,
                        "then" => Tok::Then,
                        "to" => Tok::To,
                        "type" => Tok::Type,
                        "until" => Tok::Until,
                        "var" => Tok::Var,
                        "while" => Tok::While,
                        "with" => Tok::With,
                        _ => Tok::Identifier(lowercase),
                    };
                    let offset_end = offset + tmp.len();
                    return Some(Ok((offset, token, offset_end)));
                }
                _ => {
                    if c.is_alphanumeric() || c.is_ascii_punctuation() || c.is_ascii_whitespace() {
                        return Some(Err(LexicalError {
                            start: offset,
                            end: offset + 1,
                            message: format!("unexpected input '{}'", c),
                        }));
                    } else {
                        return Some(Err(LexicalError {
                            start: offset,
                            end: offset + 1,
                            message: format!("unexpected input, codepoint U+{:04x}", c as usize),
                        }));
                    }
                }
            }
        }
    }
}

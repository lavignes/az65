use std::{
    cell::RefCell,
    fmt::{self, Debug, Display, Formatter},
    io::Read,
    marker::PhantomData,
    rc::Rc,
};

use crate::{
    charreader::{CharReader, CharReaderError},
    intern::{PathRef, StrInterner, StrRef},
};

#[derive(Copy, Clone, Debug)]
pub struct SourceLoc {
    pub pathref: PathRef,
    pub line: usize,
    pub column: usize,
}

#[derive(thiserror::Error, Debug)]
pub enum LexerError {
    #[error("read error: {source}")]
    ReadError {
        loc: SourceLoc,
        source: CharReaderError,
    },

    #[error("unexpected line break")]
    UnexpectedLineBreak { loc: SourceLoc },

    #[error("unrecognized string escape: `{msg}`")]
    UnrecognizedStringEscape { loc: SourceLoc, msg: String },

    #[error("malformed character literal: `{msg}`")]
    MalformedCharacterLiteral { loc: SourceLoc, msg: String },

    #[error("malformed binary number: `{msg}`")]
    MalformedBinaryNumber { loc: SourceLoc, msg: String },

    #[error("malformed decimal number: `{msg}`")]
    MalformedDecimalNumber { loc: SourceLoc, msg: String },

    #[error("malformed hexadecimal number: `{msg}`")]
    MalformedHexidecimalNumber { loc: SourceLoc, msg: String },

    #[error("unrecognized input: `{msg}`")]
    UnrecognizedInput { loc: SourceLoc, msg: String },

    #[error("unknown directive: `{msg}`")]
    UnknownDirective { loc: SourceLoc, msg: String },

    #[error("malformed label: `{msg}`")]
    MalformedLabel { loc: SourceLoc, msg: String },
}

impl LexerError {
    #[inline]
    pub fn loc(&self) -> SourceLoc {
        match self {
            Self::ReadError { loc, .. } => *loc,
            Self::UnexpectedLineBreak { loc } => *loc,
            Self::UnrecognizedStringEscape { loc, .. } => *loc,
            Self::MalformedCharacterLiteral { loc, .. } => *loc,
            Self::MalformedBinaryNumber { loc, .. } => *loc,
            Self::MalformedDecimalNumber { loc, .. } => *loc,
            Self::MalformedHexidecimalNumber { loc, .. } => *loc,
            Self::UnrecognizedInput { loc, .. } => *loc,
            Self::UnknownDirective { loc, .. } => *loc,
            Self::MalformedLabel { loc, .. } => *loc,
        }
    }
}

/// Architecture-specific token types
pub trait ArchTokens: Copy + Clone {
    type RegisterName: RegisterName;
    type FlagName: FlagName;
    type OperationName: OperationName;
}

pub trait RegisterName: Copy + Display + Debug + Eq {
    fn parse<S: AsRef<str>>(s: S) -> Option<Self>;
}

pub trait FlagName: Copy + Display + Debug + Eq {
    fn parse<S: AsRef<str>>(s: S) -> Option<Self>;
}

pub trait OperationName: Copy + Display + Debug + Eq {
    fn parse<S: AsRef<str>>(s: S) -> Option<Self>;
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SymbolName {
    Tilde,
    Bang,
    Mod,
    Caret,
    Ampersand,
    DoubleAmpersand,
    Star,
    Hash,
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    Minus,
    Equal,
    NotEqual,
    Plus,
    Pipe,
    DoublePipe,
    Colon,
    Comma,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    ShiftLeft,
    ShiftRight,
    ShiftLeftLogical,
    ShiftRightLogical,
    Div,
    BackSlash,
    Question,
}

impl SymbolName {
    pub fn parse<S: AsRef<str>>(s: S) -> Option<Self> {
        match s.as_ref() {
            "~" => Some(Self::Tilde),
            "!" => Some(Self::Bang),
            "%" => Some(Self::Mod),
            "^" => Some(Self::Caret),
            "&" => Some(Self::Ampersand),
            "&&" => Some(Self::DoubleAmpersand),
            "*" => Some(Self::Star),
            "#" => Some(Self::Hash),
            "(" => Some(Self::ParenOpen),
            ")" => Some(Self::ParenClose),
            "{" => Some(Self::BraceOpen),
            "}" => Some(Self::BraceClose),
            "-" => Some(Self::Minus),
            "==" => Some(Self::Equal),
            "!=" => Some(Self::NotEqual),
            "+" => Some(Self::Plus),
            "|" => Some(Self::Pipe),
            "||" => Some(Self::DoublePipe),
            ":" => Some(Self::Colon),
            "," => Some(Self::Comma),
            "<" => Some(Self::LessThan),
            ">" => Some(Self::GreaterThan),
            "<=" => Some(Self::LessEqual),
            ">=" => Some(Self::GreaterEqual),
            "<<" => Some(Self::ShiftLeft),
            ">>" => Some(Self::ShiftRight),
            "<<<" => Some(Self::ShiftLeftLogical),
            ">>>" => Some(Self::ShiftRightLogical),
            "/" => Some(Self::Div),
            "\\" => Some(Self::BackSlash),
            "?" => Some(Self::Question),
            _ => None,
        }
    }
}

impl Display for SymbolName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Tilde => "~",
                Self::Bang => "!",
                Self::Mod => " %",
                Self::Caret => "^",
                Self::Ampersand => "&",
                Self::DoubleAmpersand => "&&",
                Self::Star => "*",
                Self::Hash => "#",
                Self::ParenOpen => "(",
                Self::ParenClose => ")",
                Self::BraceOpen => "{",
                Self::BraceClose => "}",
                Self::Minus => "-",
                Self::Equal => "==",
                Self::NotEqual => "!=",
                Self::Plus => "+",
                Self::Pipe => "|",
                Self::DoublePipe => "||",
                Self::Colon => ":",
                Self::Comma => ",",
                Self::LessThan => "<",
                Self::GreaterThan => ">",
                Self::LessEqual => "<=",
                Self::GreaterEqual => ">=",
                Self::ShiftLeft => "<<",
                Self::ShiftRight => ">>",
                Self::ShiftLeftLogical => "<<<",
                Self::ShiftRightLogical => ">>>",
                Self::Div => "/",
                Self::BackSlash => "\\",
                Self::Question => "?",
            }
        )
    }
}

#[derive(Debug, Copy, Clone)]
pub enum DirectiveName {
    Org,
    Here,
    Macro,
    EndMacro,
    Defl,
    Defn,
    ReDefl,
    ReDefn,
    IsDef,
    UnDef,
    Echo,
    Die,
    Assert,
    Db,
    Dw,
    Ds,
    Include,
    Incbin,
    Struct,
    EndStruct,
    SizeOf,
    Align,
    String,
    Bin,
    Hex,
    Label,
    Meta,
    GetMeta,
    EndMeta,
    Each,
    EndEach,
    Count,
    Parse,
    Segment,
    If,
    EndIf,
    Entropy,
}

impl DirectiveName {
    fn parse(name: &str) -> Option<Self> {
        match name {
            "@org" | "@ORG" => Some(Self::Org),
            "@here" | "@HERE" => Some(Self::Here),
            "@macro" | "@MACRO" => Some(Self::Macro),
            "@endmacro" | "@ENDMACRO" => Some(Self::EndMacro),
            "@defl" | "@DEFl" => Some(Self::Defl),
            "@defn" | "@DEFN" => Some(Self::Defn),
            "@redefl" | "@REDEFl" => Some(Self::ReDefl),
            "@redefn" | "@REDEFN" => Some(Self::ReDefn),
            "@isdef" | "@ISDEF" => Some(Self::IsDef),
            "@undef" | "@UNDEF" => Some(Self::UnDef),
            "@echo" | "@ECHO" => Some(Self::Echo),
            "@die" | "@DIE" => Some(Self::Die),
            "@assert" | "@ASSERT" => Some(Self::Assert),
            "@db" | "@DB" => Some(Self::Db),
            "@dw" | "@DW" => Some(Self::Dw),
            "@ds" | "@DS" => Some(Self::Ds),
            "@include" | "@INCLUDE" => Some(Self::Include),
            "@incbin" | "@INCBIN" => Some(Self::Incbin),
            "@struct" | "@STRUCT" => Some(Self::Struct),
            "@endstruct" | "@ENDSTRUCT" => Some(Self::EndStruct),
            "@sizeof" | "@SIZEOF" => Some(Self::SizeOf),
            "@align" | "@ALIGN" => Some(Self::Align),
            "@string" | "@STRING" => Some(Self::String),
            "@bin" | "@BIN" => Some(Self::Bin),
            "@hex" | "@HEX" => Some(Self::Hex),
            "@label" | "@LABEL" => Some(Self::Label),
            "@meta" | "@META" => Some(Self::Meta),
            "@getmeta" | "@GETMETA" => Some(Self::GetMeta),
            "@endmeta" | "@ENDMETA" => Some(Self::EndMeta),
            "@each" | "@EACH" => Some(Self::Each),
            "@endeach" | "@ENDEACH" => Some(Self::EndEach),
            "@count" | "@COUNT" => Some(Self::Count),
            "@parse" | "@PARSE" => Some(Self::Parse),
            "@segment" | "@SEGMENT" => Some(Self::Segment),
            "@if" | "@IF" => Some(Self::If),
            "@endif" | "@ENDIF" => Some(Self::EndIf),
            "@entropy" | "@ENTROPY" => Some(Self::Entropy),
            _ => None,
        }
    }
}

impl Display for DirectiveName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Org => "@org",
                Self::Here => "@here",
                Self::Macro => "@macro",
                Self::EndMacro => "@endmacro",
                Self::Defl => "@defl",
                Self::Defn => "@defn",
                Self::ReDefl => "@redefl",
                Self::ReDefn => "@redefn",
                Self::IsDef => "@isdef",
                Self::UnDef => "@undef",
                Self::Echo => "@echo",
                Self::Die => "@die",
                Self::Assert => "@assert",
                Self::Db => "@db",
                Self::Dw => "@dw",
                Self::Ds => "@ds",
                Self::Include => "@include",
                Self::Incbin => "@incbin",
                Self::Struct => "@struct",
                Self::EndStruct => "@endstruct",
                Self::SizeOf => "@sizeof",
                Self::Align => "@align",
                Self::String => "@string",
                Self::Bin => "@bin",
                Self::Hex => "@hex",
                Self::Label => "@label",
                Self::Meta => "@meta",
                Self::GetMeta => "@getmeta",
                Self::EndMeta => "@endmeta",
                Self::Each => "@each",
                Self::EndEach => "@endeach",
                Self::Count => "@count",
                Self::Parse => "@parse",
                Self::Segment => "@segment",
                Self::If => "@if",
                Self::EndIf => "@endif",
                Self::Entropy => "@entropy",
            }
        )
    }
}

enum State {
    Initial,
    InComment,
    InString,
    InStringEscape,
    InHexStringEscape1,
    InHexStringEscape2,
    InChar,
    InCharEscape,
    InHexCharEscape1,
    InHexCharEscape2,
    InNumberBase2,
    InNumberBase10,
    InNumberBase16,
    InSymbol,
    InShiftSymbol,
    InIdentifier,
    InDirective,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum LabelKind {
    Global,
    Local,
    Direct,
}

#[derive(Debug, Copy, Clone)]
pub enum Token<A: ArchTokens> {
    Comment {
        loc: SourceLoc,
    },
    NewLine {
        loc: SourceLoc,
    },
    String {
        loc: SourceLoc,
        value: StrRef,
    },
    Number {
        loc: SourceLoc,
        value: u32,
    },
    Operation {
        loc: SourceLoc,
        name: A::OperationName,
    },
    Directive {
        loc: SourceLoc,
        name: DirectiveName,
    },
    Register {
        loc: SourceLoc,
        name: A::RegisterName,
    },
    Flag {
        loc: SourceLoc,
        name: A::FlagName,
    },
    Symbol {
        loc: SourceLoc,
        name: SymbolName,
    },
    Label {
        loc: SourceLoc,
        kind: LabelKind,
        value: StrRef,
    },
}

impl<A: ArchTokens> Token<A> {
    #[inline]
    pub fn loc(&self) -> SourceLoc {
        match self {
            Self::NewLine { loc, .. } => *loc,
            Self::Comment { loc, .. } => *loc,
            Self::String { loc, .. } => *loc,
            Self::Number { loc, .. } => *loc,
            Self::Operation { loc, .. } => *loc,
            Self::Directive { loc, .. } => *loc,
            Self::Register { loc, .. } => *loc,
            Self::Flag { loc, .. } => *loc,
            Self::Symbol { loc, .. } => *loc,
            Self::Label { loc, .. } => *loc,
        }
    }

    #[inline]
    pub fn as_display<'a>(
        &'a self,
        str_interner: &'a Rc<RefCell<StrInterner>>,
    ) -> DisplayToken<'a, A> {
        DisplayToken {
            inner: self,
            str_interner,
        }
    }
}

pub struct DisplayToken<'a, A: ArchTokens> {
    inner: &'a Token<A>,
    str_interner: &'a Rc<RefCell<StrInterner>>,
}

impl<'a, A: ArchTokens> Display for DisplayToken<'a, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self {
            inner,
            str_interner,
        } = *self;
        match *inner {
            Token::NewLine { .. } => write!(f, "line break"),
            Token::Comment { .. } => write!(f, "comment"),
            Token::String { value, .. } => {
                let str_interner = str_interner.as_ref().borrow();
                let value = str_interner.get(value).unwrap();
                write!(f, "string: \"{value}\"")
            }
            Token::Number { value, .. } => write!(f, "number: {value}"),
            Token::Operation { name, .. } => write!(f, "operation: \"{name}\""),
            Token::Directive { name, .. } => write!(f, "directive: \"{name}\""),
            Token::Register { name, .. } => write!(f, "register: \"{name}\""),
            Token::Flag { name, .. } => write!(f, "flag: \"{name}\""),
            Token::Symbol { name, .. } => write!(f, "symbol: \"{name}\""),
            Token::Label { value, .. } => {
                let str_interner = str_interner.as_ref().borrow();
                let value = str_interner.get(value).unwrap();
                write!(f, "label: \"{value}\"")
            }
        }
    }
}

pub struct Lexer<R, A> {
    str_interner: Rc<RefCell<StrInterner>>,
    loc: SourceLoc,
    tok_loc: SourceLoc,
    included_from: Option<SourceLoc>,
    inner: CharReader<R>,
    stash: Option<char>,
    state: State,
    buffer: String,
    eof: bool,

    marker: PhantomData<A>,
}

impl<R: Read, A> Lexer<R, A> {
    #[inline]
    pub fn new(
        str_interner: Rc<RefCell<StrInterner>>,
        included_from: Option<SourceLoc>,
        pathref: PathRef,
        reader: R,
    ) -> Self {
        let loc = SourceLoc {
            pathref,
            line: 1,
            column: 0,
        };
        Self {
            str_interner,
            loc,
            tok_loc: loc,
            included_from,
            inner: CharReader::new(reader),
            stash: None,
            state: State::Initial,
            buffer: String::new(),
            eof: false,
            marker: PhantomData,
        }
    }

    #[inline]
    pub fn loc(&self) -> SourceLoc {
        self.loc
    }

    #[inline]
    pub fn included_from(&self) -> Option<SourceLoc> {
        self.included_from
    }

    #[cfg(test)]
    #[inline]
    fn str_interner_mut(&self) -> std::cell::RefMut<StrInterner> {
        self.str_interner.borrow_mut()
    }

    #[inline]
    fn is_value_terminator(&self, c: char) -> bool {
        // Basically any char that could reasonably mark the end of a label or number
        matches!(
            c,
            '~' | '!'
                | '%'
                | '^'
                | '&'
                | '*'
                | '-'
                | '+'
                | '>'
                | '<'
                | '='
                | '?'
                | '/'
                | ':'
                | '|'
                | ';'
                | ','
                | '@'
                | ')'
                | '}'
                | '#'
                | '\\'
                | '\n'
        )
    }

    #[inline]
    fn is_symbol_start(&self, c: char) -> bool {
        matches!(
            c,
            '~' | '!'
                | '%'
                | '^'
                | '&'
                | '*'
                | '#'
                | '('
                | ')'
                | '{'
                | '}'
                | '-'
                | '='
                | '+'
                | '|'
                | ':'
                | ','
                | '<'
                | '>'
                | '?'
                | '/'
                | '\\'
        )
    }
}

impl<R: Read, A: ArchTokens> Iterator for Lexer<R, A> {
    type Item = Result<Token<A>, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let c = match self.stash.take() {
                Some(c) => c,
                None => match self.inner.next() {
                    None => {
                        // TODO: This is a hack to flush any tokens still in the buffers
                        //   pretend there is one final newline at the end of input.
                        if self.eof {
                            return None;
                        }
                        self.eof = true;
                        self.loc.line += 1;
                        self.loc.column = 0;
                        '\n'
                    }
                    Some(Err(e)) => {
                        return Some(Err(LexerError::ReadError {
                            loc: self.loc,
                            source: e,
                        }));
                    }
                    Some(Ok(c)) => {
                        self.loc.column += 1;
                        if c == '\n' {
                            self.loc.line += 1;
                            self.loc.column = 0;
                        }
                        c
                    }
                },
            };

            match self.state {
                State::Initial => match c {
                    '\n' => return Some(Ok(Token::NewLine { loc: self.loc })),

                    _ if c.is_whitespace() => continue,

                    ';' => {
                        self.state = State::InComment;
                        self.tok_loc = self.loc;
                    }

                    '"' => {
                        self.state = State::InString;
                        self.tok_loc = self.loc;
                        self.buffer.clear();
                    }

                    '\'' => {
                        self.state = State::InChar;
                        self.tok_loc = self.loc;
                        self.buffer.clear();
                    }

                    '%' => {
                        self.state = State::InNumberBase2;
                        self.tok_loc = self.loc;
                        self.buffer.clear();
                    }

                    '0'..='9' => {
                        self.state = State::InNumberBase10;
                        self.tok_loc = self.loc;
                        self.buffer.clear();
                        self.buffer.push(c);
                    }

                    '$' => {
                        self.state = State::InNumberBase16;
                        self.tok_loc = self.loc;
                        self.buffer.clear();
                    }

                    _ if self.is_symbol_start(c) => {
                        self.state = State::InSymbol;
                        self.tok_loc = self.loc;
                        self.buffer.clear();
                        self.buffer.push(c);
                    }

                    '@' => {
                        self.state = State::InDirective;
                        self.tok_loc = self.loc;
                        self.buffer.clear();
                        self.buffer.push(c);
                    }

                    _ if c.is_alphanumeric() || c == '_' || c == '.' => {
                        self.state = State::InIdentifier;
                        self.tok_loc = self.loc;
                        self.buffer.clear();
                        self.buffer.push(c);
                    }

                    _ => {
                        return Some(Err(LexerError::UnrecognizedInput {
                            loc: self.loc,
                            msg: format!("{c}"),
                        }));
                    }
                },

                State::InComment => {
                    if c == '\n' {
                        self.state = State::Initial;
                        self.stash = Some(c);
                        return Some(Ok(Token::Comment { loc: self.tok_loc }));
                    }
                }

                State::InString => match c {
                    '\n' => {
                        return Some(Err(LexerError::UnexpectedLineBreak { loc: self.loc }));
                    }

                    '"' => {
                        self.state = State::Initial;
                        let value = self.str_interner.borrow_mut().intern(&self.buffer);
                        return Some(Ok(Token::String {
                            loc: self.tok_loc,
                            value,
                        }));
                    }

                    '\\' => {
                        self.state = State::InStringEscape;
                    }

                    _ => self.buffer.push(c),
                },

                State::InStringEscape => match c {
                    '\n' => {
                        self.state = State::InString;
                    }

                    'n' => {
                        self.state = State::InString;
                        self.buffer.push('\n');
                    }

                    'r' => {
                        self.state = State::InString;
                        self.buffer.push('\r');
                    }

                    't' => {
                        self.state = State::InString;
                        self.buffer.push('\t');
                    }

                    '\\' => {
                        self.state = State::InString;
                        self.buffer.push('\\');
                    }

                    '0' => {
                        self.state = State::InString;
                        self.buffer.push('\0');
                    }

                    '\"' => {
                        self.state = State::InString;
                        self.buffer.push('\"');
                    }

                    '$' => {
                        self.state = State::InHexStringEscape1;
                    }

                    _ => {
                        return Some(Err(LexerError::UnrecognizedStringEscape {
                            loc: self.loc,
                            msg: format!("\\{c}"),
                        }));
                    }
                },

                State::InHexStringEscape1 => match c {
                    '\n' => {
                        return Some(Err(LexerError::UnexpectedLineBreak { loc: self.loc }));
                    }

                    '0'..='9' | 'a'..='f' | 'A'..='F' => {
                        self.state = State::InHexStringEscape2;
                        self.buffer.push(c.to_ascii_lowercase());
                    }

                    _ => {
                        return Some(Err(LexerError::UnrecognizedStringEscape {
                            loc: self.loc,
                            msg: format!("\\${c}"),
                        }));
                    }
                },

                State::InHexStringEscape2 => match c {
                    '\n' => {
                        return Some(Err(LexerError::UnexpectedLineBreak { loc: self.loc }));
                    }

                    '0'..='9' | 'a'..='f' | 'A'..='F' => {
                        // Its kinda janky, but we pushed both bytes onto the buffer
                        // so we can parse them, then we overwrite the bytes with the char
                        // value.
                        self.state = State::InString;
                        self.buffer.push(c.to_ascii_lowercase());

                        let last2 = &self.buffer[self.buffer.len() - 2..self.buffer.len()];
                        let byte = u8::from_str_radix(last2, 16).unwrap();

                        self.buffer.truncate(self.buffer.len() - 2);
                        self.buffer.push(byte as char);
                    }

                    _ => {
                        return Some(Err(LexerError::UnrecognizedStringEscape {
                            loc: self.tok_loc,
                            msg: format!("\\${}{}", self.buffer.chars().last().unwrap(), c),
                        }));
                    }
                },

                State::InChar => match c {
                    '\n' => {
                        return Some(Err(LexerError::UnexpectedLineBreak { loc: self.loc }));
                    }

                    '\'' => {
                        self.state = State::Initial;
                        let len = self.buffer.len();
                        if len == 0 {
                            return Some(Err(LexerError::MalformedCharacterLiteral {
                                loc: self.tok_loc,
                                msg: "Character literal cannot have 0 length".into(),
                            }));
                        }
                        if len > 4 {
                            return Some(Err(LexerError::MalformedCharacterLiteral {
                                loc: self.tok_loc,
                                msg: format!(
                                    "Character literal length ({len}) is larger than 4 bytes"
                                ),
                            }));
                        }
                        let mut bytes = [0; 4];
                        for (i, b) in self.buffer.as_bytes().iter().enumerate() {
                            bytes[i] = *b;
                        }
                        let value = u32::from_le_bytes(bytes);
                        return Some(Ok(Token::Number {
                            loc: self.tok_loc,
                            value,
                        }));
                    }

                    '\\' => {
                        self.state = State::InCharEscape;
                    }

                    _ => self.buffer.push(c),
                },

                State::InCharEscape => match c {
                    '\n' => {
                        self.state = State::InChar;
                    }

                    'n' => {
                        self.state = State::InChar;
                        self.buffer.push('\n');
                    }

                    'r' => {
                        self.state = State::InChar;
                        self.buffer.push('\r');
                    }

                    't' => {
                        self.state = State::InChar;
                        self.buffer.push('\t');
                    }

                    '\\' => {
                        self.state = State::InChar;
                        self.buffer.push('\\');
                    }

                    '0' => {
                        self.state = State::InChar;
                        self.buffer.push('\0');
                    }

                    '\"' => {
                        self.state = State::InChar;
                        self.buffer.push('\"');
                    }

                    '$' => {
                        self.state = State::InHexCharEscape1;
                    }

                    _ => {
                        return Some(Err(LexerError::UnrecognizedStringEscape {
                            loc: self.loc,
                            msg: format!("\\{c}"),
                        }));
                    }
                },

                State::InHexCharEscape1 => match c {
                    '\n' => {
                        return Some(Err(LexerError::UnexpectedLineBreak { loc: self.loc }));
                    }

                    '0'..='9' | 'a'..='f' | 'A'..='F' => {
                        self.state = State::InHexCharEscape2;
                        self.buffer.push(c.to_ascii_lowercase());
                    }

                    _ => {
                        return Some(Err(LexerError::UnrecognizedStringEscape {
                            loc: self.loc,
                            msg: format!("\\${c}"),
                        }));
                    }
                },

                State::InHexCharEscape2 => match c {
                    '\n' => {
                        return Some(Err(LexerError::UnexpectedLineBreak { loc: self.loc }));
                    }

                    '0'..='9' | 'a'..='f' | 'A'..='F' => {
                        // Its kinda janky, but we pushed both bytes onto the buffer
                        // so we can parse them, then we overwrite the bytes with the char
                        // value.
                        self.state = State::InChar;
                        self.buffer.push(c.to_ascii_lowercase());

                        let last2 = &self.buffer[self.buffer.len() - 2..self.buffer.len()];
                        let byte = u8::from_str_radix(last2, 16).unwrap();

                        self.buffer.truncate(self.buffer.len() - 2);
                        self.buffer.push(byte as char);
                    }

                    _ => {
                        return Some(Err(LexerError::UnrecognizedStringEscape {
                            loc: self.tok_loc,
                            msg: format!("\\${}{}", self.buffer.chars().last().unwrap(), c),
                        }));
                    }
                },

                State::InNumberBase2 => match c {
                    _ if c.is_whitespace() || self.is_value_terminator(c) => {
                        self.state = State::Initial;
                        self.stash = Some(c);

                        // Instead of treating this as a binary number, this must
                        // be the mod symbol
                        if self.buffer.is_empty() {
                            return Some(Ok(Token::Symbol {
                                loc: self.tok_loc,
                                name: SymbolName::Mod,
                            }));
                        }

                        let value = match u32::from_str_radix(&self.buffer, 2) {
                            Ok(value) => value,
                            Err(_) => {
                                return Some(Err(LexerError::MalformedBinaryNumber {
                                    loc: self.tok_loc,
                                    msg: format!("%{}{}", self.buffer, c),
                                }))
                            }
                        };
                        return Some(Ok(Token::Number {
                            loc: self.tok_loc,
                            value,
                        }));
                    }

                    '0' | '1' => self.buffer.push(c),

                    _ => {
                        return Some(Err(LexerError::MalformedBinaryNumber {
                            loc: self.tok_loc,
                            msg: format!("%{}{}", self.buffer, c),
                        }));
                    }
                },

                State::InNumberBase10 => match c {
                    _ if c.is_whitespace() || self.is_value_terminator(c) => {
                        self.state = State::Initial;
                        self.stash = Some(c);

                        let value = match u32::from_str_radix(&self.buffer, 10) {
                            Ok(value) => value,
                            Err(_) => {
                                return Some(Err(LexerError::MalformedDecimalNumber {
                                    loc: self.tok_loc,
                                    msg: format!("{}{}", self.buffer, c),
                                }))
                            }
                        };
                        return Some(Ok(Token::Number {
                            loc: self.tok_loc,
                            value,
                        }));
                    }

                    '0'..='9' => self.buffer.push(c),

                    _ => {
                        return Some(Err(LexerError::MalformedDecimalNumber {
                            loc: self.tok_loc,
                            msg: format!("{}{}", self.buffer, c),
                        }));
                    }
                },

                State::InNumberBase16 => match c {
                    _ if c.is_whitespace() || self.is_value_terminator(c) => {
                        self.state = State::Initial;
                        self.stash = Some(c);

                        let value = match u32::from_str_radix(&self.buffer, 16) {
                            Ok(value) => value,
                            Err(_) => {
                                return Some(Err(LexerError::MalformedHexidecimalNumber {
                                    loc: self.tok_loc,
                                    msg: format!("${}{}", self.buffer, c),
                                }))
                            }
                        };
                        return Some(Ok(Token::Number {
                            loc: self.tok_loc,
                            value,
                        }));
                    }

                    '0'..='9' | 'a'..='f' | 'A'..='F' => self.buffer.push(c.to_ascii_lowercase()),

                    _ => {
                        return Some(Err(LexerError::MalformedHexidecimalNumber {
                            loc: self.tok_loc,
                            msg: format!("${}{}", self.buffer, c),
                        }));
                    }
                },

                State::InSymbol => {
                    self.state = State::Initial;
                    self.buffer.push(c);

                    // first try and parse a 2 char symbol
                    if let Some(name) = SymbolName::parse(&self.buffer) {
                        // The shift symbols may possibly be 3 chars. So drop into those states next
                        match name {
                            SymbolName::ShiftLeft | SymbolName::ShiftRight => {
                                self.state = State::InShiftSymbol;
                                continue;
                            }

                            _ => {
                                return Some(Ok(Token::Symbol {
                                    loc: self.tok_loc,
                                    name,
                                }));
                            }
                        }
                    }

                    // It must be 1 char (stash the other char)
                    self.stash = self.buffer.pop();
                    let name = SymbolName::parse(&self.buffer).unwrap();
                    return Some(Ok(Token::Symbol {
                        loc: self.tok_loc,
                        name,
                    }));
                }

                State::InShiftSymbol => {
                    self.state = State::Initial;
                    self.buffer.push(c);

                    // It must be a logical shift
                    if let Some(name) = SymbolName::parse(&self.buffer) {
                        return Some(Ok(Token::Symbol {
                            loc: self.tok_loc,
                            name,
                        }));
                    }

                    // It must be a arithmetic shift (stash the other char)
                    self.stash = self.buffer.pop();
                    let name = SymbolName::parse(&self.buffer).unwrap();
                    return Some(Ok(Token::Symbol {
                        loc: self.tok_loc,
                        name,
                    }));
                }

                State::InDirective => match c {
                    _ if c.is_alphanumeric() || c == '_' => {
                        self.buffer.push(c);
                    }

                    _ => {
                        self.state = State::Initial;
                        self.stash = Some(c);

                        if let Some(name) = DirectiveName::parse(&self.buffer) {
                            return Some(Ok(Token::Directive {
                                loc: self.tok_loc,
                                name,
                            }));
                        }

                        return Some(Err(LexerError::UnknownDirective {
                            loc: self.tok_loc,
                            msg: self.buffer.clone(),
                        }));
                    }
                },

                State::InIdentifier => match c {
                    _ if c.is_alphanumeric() || c == '_' || c == '.' => {
                        self.buffer.push(c);
                    }

                    _ => {
                        self.state = State::Initial;
                        self.stash = Some(c);

                        if let Some(name) = A::OperationName::parse(&self.buffer) {
                            return Some(Ok(Token::Operation {
                                loc: self.tok_loc,
                                name,
                            }));
                        }

                        if let Some(name) = A::RegisterName::parse(&self.buffer) {
                            // FIXME: hack for z80 af' register
                            if c == '\'' {
                                self.buffer.push(c);
                                if let Some(name) = A::RegisterName::parse(&self.buffer) {
                                    self.stash = None;
                                    return Some(Ok(Token::Register {
                                        loc: self.tok_loc,
                                        name,
                                    }));
                                }
                                self.buffer.pop();
                            }

                            return Some(Ok(Token::Register {
                                loc: self.tok_loc,
                                name,
                            }));
                        }

                        if let Some(name) = A::FlagName::parse(&self.buffer) {
                            return Some(Ok(Token::Flag {
                                loc: self.tok_loc,
                                name,
                            }));
                        }

                        let value = self.str_interner.borrow_mut().intern(&self.buffer);
                        return match self.buffer.chars().filter(|c| *c == '.').count() {
                            0 => Some(Ok(Token::Label {
                                loc: self.tok_loc,
                                kind: LabelKind::Global,
                                value,
                            })),
                            1 => {
                                if self.buffer.starts_with('.') {
                                    Some(Ok(Token::Label {
                                        loc: self.tok_loc,
                                        kind: LabelKind::Local,
                                        value,
                                    }))
                                } else {
                                    Some(Ok(Token::Label {
                                        loc: self.tok_loc,
                                        kind: LabelKind::Direct,
                                        value,
                                    }))
                                }
                            }
                            _ => Some(Err(LexerError::MalformedLabel {
                                loc: self.tok_loc,
                                msg: self.buffer.clone(),
                            })),
                        };
                    }
                },
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::*;
    use crate::intern::PathInterner;

    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    struct TestName;

    impl Display for TestName {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            write!(f, "nop")
        }
    }

    impl RegisterName for TestName {
        fn parse<S: AsRef<str>>(_: S) -> Option<Self> {
            None
        }
    }

    impl FlagName for TestName {
        fn parse<S: AsRef<str>>(_: S) -> Option<Self> {
            None
        }
    }

    impl OperationName for TestName {
        fn parse<S: AsRef<str>>(s: S) -> Option<Self> {
            match s.as_ref() {
                "nop" => Some(Self),
                _ => None,
            }
        }
    }

    #[derive(Copy, Clone)]
    struct TestArchTokens;

    impl ArchTokens for TestArchTokens {
        type RegisterName = TestName;
        type FlagName = TestName;
        type OperationName = TestName;
    }

    fn lexer(text: &str) -> Lexer<Cursor<&str>, TestArchTokens> {
        let path_interner = Rc::new(RefCell::new(PathInterner::new()));
        let str_interner = Rc::new(RefCell::new(StrInterner::new()));
        let pathref = path_interner.borrow_mut().intern("file.test");
        Lexer::new(str_interner, None, pathref, Cursor::new(text))
    }

    #[test]
    fn comment() {
        let text = r#"
            ; comment
        "#;
        let mut lexer = lexer(text);
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), Some(Ok(Token::Comment { .. }))));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), None));
    }

    #[test]
    fn string() {
        let text = r#"
            "test"
        "#;
        let mut lexer = lexer(text);
        let string = lexer.str_interner_mut().intern("test");
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::String { value, .. })) if value == string
        ));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), None));
    }

    #[test]
    fn string_escape() {
        let text = r#"
            "test\n"
        "#;
        let mut lexer = lexer(text);
        let string = lexer.str_interner_mut().intern("test\n");
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::String { value, .. })) if value == string
        ));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), None));
    }

    #[test]
    fn string_escape_raw() {
        let text = r#"
            "test\$7f"
        "#;
        let mut lexer = lexer(text);
        let string = lexer.str_interner_mut().intern("test\x7f");
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::String { value, .. })) if value == string
        ));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), None));
    }

    #[test]
    fn char() {
        let text = r#"
            'q'
        "#;
        let mut lexer = lexer(text);
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Number {
                value,
                ..
            })) if value == ('q' as u32)
        ));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), None));
    }

    #[test]
    fn char_escape() {
        let text = r#"
            '\n'
        "#;
        let mut lexer = lexer(text);
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Number { value, .. })) if value == ('\n' as u32)
        ));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), None));
    }

    #[test]
    fn char_escape_raw() {
        let text = r#"
            '\$7f'
        "#;
        let mut lexer = lexer(text);
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Number { value, .. })) if value == ('\x7f' as u32)
        ));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), None));
    }

    #[test]
    fn base2() {
        let text = r#"
            %010101
        "#;
        let mut lexer = lexer(text);
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Number {
                value: 0b010101,
                ..
            }))
        ));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), None));
    }

    #[test]
    fn base10() {
        let text = r#"
            123456
        "#;
        let mut lexer = lexer(text);
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Number { value: 123456, .. }))
        ));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), None));
    }

    #[test]
    fn base16() {
        let text = r#"
            $cafebabe
        "#;
        let mut lexer = lexer(text);
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Number {
                value: 0xcafebabe,
                ..
            }))
        ));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), None));
    }

    #[test]
    fn symbols() {
        let text = r#"
            ~ ! % ^ & && * # ( ) { } - == + | || : , < > <= >= << >> <<< >>> / \ ?
        "#;
        let mut lexer = lexer(text);
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::Tilde,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::Bang,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::Mod,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::Caret,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::Ampersand,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::DoubleAmpersand,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::Star,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::Hash,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::ParenOpen,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::ParenClose,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::BraceOpen,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::BraceClose,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::Minus,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::Equal,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::Plus,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::Pipe,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::DoublePipe,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::Colon,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::Comma,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::LessThan,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::GreaterThan,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::LessEqual,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::GreaterEqual,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::ShiftLeft,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::ShiftRight,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::ShiftLeftLogical,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::ShiftRightLogical,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::Div,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::BackSlash,
                ..
            }))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Symbol {
                name: SymbolName::Question,
                ..
            }))
        ));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), None));
    }

    #[test]
    fn labels() {
        let text = r#"
            global_label
            .local_label
            direct.label
        "#;
        let mut lexer = lexer(text);
        let label = lexer.str_interner_mut().intern("global_label");
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Label {
                kind: LabelKind::Global,
                value,
                ..
            })) if value == label
        ));
        let label = lexer.str_interner_mut().intern(".local_label");
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Label {
                kind: LabelKind::Local,
                value,
                ..
            })) if value == label
        ));
        let label = lexer.str_interner_mut().intern("direct.label");
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(
            lexer.next(),
            Some(Ok(Token::Label {
                kind: LabelKind::Direct,
                value,
                ..
            })) if value == label
        ));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), Some(Ok(Token::NewLine { .. }))));
        assert!(matches!(lexer.next(), None));
    }
}

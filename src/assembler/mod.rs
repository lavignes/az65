use std::{
    borrow::Borrow,
    cell::RefCell,
    fmt::Write,
    io::{Cursor, Read},
    iter,
    path::Path,
    rc::Rc,
};

use fxhash::FxHashMap;

use crate::{
    expr::{Expr, ExprNode},
    fileman::{FileManager, FileSystem},
    intern::{PathRef, StrInterner, StrRef},
    lexer::{DirectiveName, LabelKind, Lexer, LexerError, SourceLoc, SymbolName, Token},
    linker::{Link, Module},
    ops::{OperationName, RegisterName},
    symtab::{Symbol, Symtab},
};

#[cfg(test)]
mod tests;

macro_rules! asm_err {
    ($loc:expr, $($arg:tt)*) => {
        Err(($loc, AssemblerError(format!($($arg)*))))
    };
}

enum TokenSource<R> {
    Lexer(Lexer<R>),
    ParseLexer(Lexer<Cursor<String>>),
    Macro(MacroState),
}

impl<R: Read> TokenSource<R> {
    pub fn loc(&self) -> SourceLoc {
        match self {
            Self::Lexer(lexer) => lexer.loc(),
            Self::ParseLexer(lexer) => lexer.loc(),
            Self::Macro(state) => state.loc,
        }
    }

    pub fn included_from(&self) -> Option<SourceLoc> {
        match self {
            Self::Lexer(lexer) => lexer.included_from(),
            Self::ParseLexer(lexer) => lexer.included_from(),
            Self::Macro(state) => state.included_from,
        }
    }

    fn next(
        &mut self,
        macros: &mut FxHashMap<StrRef, Macro>,
    ) -> Option<Result<Token, (SourceLoc, AssemblerError)>> {
        match self {
            Self::Lexer(lexer) => lexer.next().map(|res| res.map_err(LexerError::into)),
            Self::ParseLexer(lexer) => lexer.next().map(|res| res.map_err(LexerError::into)),
            Self::Macro(state) => {
                let mac = macros.get_mut(&state.name).unwrap();

                loop {
                    if state.macro_offset >= mac.tokens.len() {
                        return None;
                    }

                    if let Some(arg) = state.expanding_macro_arg {
                        let arg_toks = &state.args[arg];
                        if state.macro_arg_offset >= arg_toks.len() {
                            state.expanding_macro_arg = None;
                            state.macro_offset += 1;
                            continue;
                        }

                        // arguments will already be expanded if they contained macro invocations
                        let tok = arg_toks[state.macro_arg_offset];
                        state.macro_arg_offset += 1;
                        state.loc = tok.loc();
                        return Some(Ok(tok));
                    }

                    let tok = mac.tokens[state.macro_offset];
                    match tok {
                        MacroToken::Token(tok) => {
                            state.macro_offset += 1;
                            state.loc = tok.loc();
                            return Some(Ok(tok));
                        }

                        MacroToken::Argument { loc, index } => {
                            if index >= mac.args {
                                let interner = state.str_interner.as_ref().borrow();
                                let name = interner.get(state.name).unwrap();
                                return Some(asm_err!(
                                    loc,
                                    "Unexpected argument index: ({index}), the macro \"{name}\" takes {} arguments",
                                    mac.args
                                ));
                            }

                            state.expanding_macro_arg = Some(index);
                            state.macro_arg_offset = 0;
                            continue;
                        }

                        MacroToken::Uniq(loc) => {
                            state.macro_offset += 1;
                            state.loc = loc;
                            let value = state.uniq as u32;
                            state.uniq += 1;
                            return Some(Ok(Token::Number { loc, value }));
                        }
                    }
                }
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum MacroToken {
    Token(Token),
    Argument { loc: SourceLoc, index: usize },
    Uniq(SourceLoc),
}

struct Macro {
    loc: SourceLoc,
    args: usize,
    tokens: Vec<MacroToken>,
}

#[derive(thiserror::Error, Debug)]
#[error("{0}")]
pub struct AssemblerError(String);

impl From<LexerError> for (SourceLoc, AssemblerError) {
    fn from(e: LexerError) -> Self {
        (e.loc(), AssemblerError(format!("{e}")))
    }
}

struct MacroState {
    name: StrRef,
    args: Vec<Vec<Token>>,
    macro_offset: usize,
    expanding_macro_arg: Option<usize>,
    macro_arg_offset: usize,
    loc: SourceLoc,
    uniq: usize,
    included_from: Option<SourceLoc>,
    str_interner: Rc<RefCell<StrInterner>>,
}

pub struct Assembler<S, R> {
    file_manager: FileManager<S>,
    str_interner: Rc<RefCell<StrInterner>>,
    token_sources: Vec<TokenSource<R>>,
    token_source: Option<TokenSource<R>>,
    cwds: Vec<PathRef>,
    cwd: Option<PathRef>,
    macros: FxHashMap<StrRef, Macro>,
    symtab: Symtab,
    data: Vec<u8>,
    links: Vec<Link>,

    uniq: usize,
    stash: Option<Token>,
    loc: Option<SourceLoc>,
    here: u32,
    active_namespace: Option<StrRef>,
    active_macro: Option<StrRef>,
}

impl<S, R> Assembler<S, R>
where
    S: FileSystem<Reader = R>,
    R: Read,
{
    pub fn new(file_system: S) -> Self {
        Self {
            file_manager: FileManager::new(file_system),
            str_interner: Rc::new(RefCell::new(StrInterner::new())),
            token_sources: Vec::new(),
            token_source: None,
            cwds: Vec::new(),
            cwd: None,
            macros: FxHashMap::default(),
            symtab: Symtab::new(),
            data: Vec::new(),
            links: Vec::new(),

            uniq: 0,
            stash: None,
            loc: None,
            here: 0,
            active_namespace: None,
            active_macro: None,
        }
    }

    pub fn add_search_path<C: AsRef<Path>, P: AsRef<Path>>(
        &mut self,
        cwd: C,
        path: P,
    ) -> Result<(), AssemblerError> {
        let path = path.as_ref();
        self.file_manager.add_search_path(cwd, path).map_err(|e| {
            AssemblerError(format!(
                "Failed to find include path \"{}\": {e}",
                path.display()
            ))
        })?;
        Ok(())
    }

    pub fn assemble<C: AsRef<Path>, P: AsRef<Path>>(
        mut self,
        cwd: C,
        path: P,
    ) -> Result<Module<S>, AssemblerError> {
        let path = path.as_ref();
        let (pathref, reader) = match self.file_manager.reader(&cwd, path) {
            Ok(Some(tup)) => tup,
            Ok(None) => {
                return Err(AssemblerError(format!(
                    "File not found: \"{}\"",
                    path.display()
                )))
            }
            Err(e) => {
                return Err(AssemblerError(format!(
                    "Failed to open \"{}\" for reading: {e}",
                    path.display()
                )))
            }
        };

        self.token_source = Some(TokenSource::Lexer(Lexer::new(
            self.str_interner.clone(),
            None,
            pathref,
            reader,
        )));
        self.cwd = Some(self.file_manager.intern(cwd, "."));

        if let Err((loc, e)) = self.parse_all() {
            return Err(self.trace_error(loc, e));
        }

        let Self {
            str_interner,
            file_manager,
            symtab,
            data,
            links,
            ..
        } = self;
        Ok(Module::new(str_interner, file_manager, symtab, data, links))
    }

    #[inline]
    fn loc(&mut self) -> SourceLoc {
        self.loc.unwrap()
    }

    fn peek(&mut self) -> Result<Option<&Token>, (SourceLoc, AssemblerError)> {
        loop {
            if self.token_source.is_none() {
                self.token_source = self.token_sources.pop();
                self.cwd = self.cwds.pop();
            }
            match self.stash {
                Some(_) => return Ok(self.stash.as_ref()),

                None => match &mut self.token_source {
                    None => return Ok(None),

                    Some(token_source) => {
                        let tok = token_source.next(&mut self.macros).transpose()?;
                        // Note: we intentionally do not skip newlines and comments.
                        // we want to pass them on to expression parsing! They act as an
                        // "epsilon" token to terminate the non-terminal expression!
                        // TODO: Need to add a way to do multi-line expressions?

                        // if we arent currently defining a macro
                        if self.active_macro.is_none() {
                            // check to see if the current label token is a macro we need to expand
                            if let Some(Token::Label {
                                kind: LabelKind::Global,
                                value,
                                ..
                            }) = tok
                            {
                                if self.macros.contains_key(&value) {
                                    let mac = self.expect_macro_invoke(value)?;
                                    self.token_sources.push(self.token_source.take().unwrap());
                                    self.cwds.push(self.cwd.clone().unwrap());

                                    self.token_sources.push(TokenSource::Macro(mac));
                                    self.cwds.push(self.cwd.take().unwrap());
                                    continue;
                                }
                            }
                        }

                        // Check for any of the macro-like directives
                        match tok {
                            Some(Token::Directive {
                                loc,
                                name: DirectiveName::String,
                            }) => {
                                self.stash = Some(self.expect_string_directive_arg(loc)?);
                                self.loc = Some(loc);
                                continue;
                            }

                            Some(Token::Directive {
                                loc,
                                name: DirectiveName::Label,
                            }) => {
                                self.stash = Some(self.expect_label_directive_arg(loc)?);
                                self.loc = Some(loc);
                                continue;
                            }

                            Some(Token::Directive {
                                loc,
                                name: DirectiveName::Count,
                            }) => {
                                let mac = self.expect_count_directive(loc)?;

                                self.token_sources.push(self.token_source.take().unwrap());
                                self.cwds.push(self.cwd.unwrap().clone());

                                self.token_sources.push(TokenSource::Macro(mac));
                                self.cwds.push(self.cwd.take().unwrap());
                                continue;
                            }

                            Some(Token::Directive {
                                loc,
                                name: DirectiveName::MetaGet,
                            }) => {
                                let direct = match self.next()? {
                                    None => return self.end_of_input_err(),

                                    Some(Token::Label { loc, value, kind }) => match kind {
                                        LabelKind::Global | LabelKind::Direct => value,

                                        LabelKind::Local => {
                                            if let Some(namespace) = self.active_namespace {
                                                let global_label = {
                                                    let interner =
                                                        self.str_interner.as_ref().borrow();
                                                    let global = interner.get(namespace).unwrap();
                                                    let label = interner.get(value).unwrap();
                                                    format!("{global}{label}")
                                                };
                                                self.str_interner.borrow_mut().intern(global_label)
                                            } else {
                                                let interner = self.str_interner.as_ref().borrow();
                                                let label = interner.get(value).unwrap();
                                                return asm_err!(loc, "The local symbol \"{label}\" is being read but there was no global label defined before it");
                                            }
                                        }
                                    },

                                    Some(tok) => {
                                        return asm_err!(
                                            tok.loc(),
                                            "Unexpected {}, expected a label",
                                            tok.as_display(&self.str_interner)
                                        )
                                    }
                                };

                                self.expect_symbol(SymbolName::Comma)?;

                                let key = match self.next()? {
                                    None => return self.end_of_input_err(),
                                    Some(Token::String { value, .. }) => value,
                                    Some(tok) => {
                                        return asm_err!(
                                            tok.loc(),
                                            "Unexpected {}, expected a metadata key",
                                            tok.as_display(&self.str_interner)
                                        );
                                    }
                                };

                                let mut toks = Vec::new();
                                if let Some(sym) = self.symtab.get(direct) {
                                    if let Some(meta) = self.symtab.meta_interner().get(sym.meta())
                                    {
                                        for item in meta {
                                            if item[0] == key {
                                                toks.push(MacroToken::Token(Token::String {
                                                    loc,
                                                    value: item[1],
                                                }));
                                            }
                                        }
                                    }
                                } else {
                                    let mut interner = self.str_interner.borrow_mut();
                                    let value = interner.intern("");
                                    toks.push(MacroToken::Token(Token::String { loc, value }));
                                }

                                let name = self
                                    .str_interner
                                    .borrow_mut()
                                    .intern(format!("@metaget Invocation{}", self.uniq));
                                self.uniq += 1;
                                self.macros.insert(
                                    name,
                                    Macro {
                                        loc,
                                        args: 0,
                                        tokens: toks,
                                    },
                                );

                                self.token_sources.push(self.token_source.take().unwrap());
                                self.cwds.push(self.cwd.unwrap().clone());

                                let uniq = self.uniq;
                                self.uniq += 1;
                                self.token_sources.push(TokenSource::Macro(MacroState {
                                    name,
                                    args: Vec::new(),
                                    macro_offset: 0,
                                    expanding_macro_arg: None,
                                    macro_arg_offset: 0,
                                    loc,
                                    uniq,
                                    included_from: Some(loc),
                                    str_interner: self.str_interner.clone(),
                                }));
                                self.cwds.push(self.cwd.take().unwrap());
                                continue;
                            }

                            Some(Token::Directive {
                                loc,
                                name: DirectiveName::Parse,
                            }) => {
                                let reader = match self.next()? {
                                    None => return self.end_of_input_err(),
                                    Some(Token::String { value, .. }) => {
                                        let interner = self.str_interner.as_ref().borrow();
                                        // TODO: Not really efficient to clone. but /shrug
                                        Cursor::new(interner.get(value).unwrap().to_owned())
                                    }
                                    Some(tok) => {
                                        return asm_err!(
                                            tok.loc(),
                                            "Unexpected {}, expected a string to parse",
                                            tok.as_display(&self.str_interner)
                                        )
                                    }
                                };

                                self.token_sources.push(self.token_source.take().unwrap());
                                self.cwds.push(self.cwd.unwrap().clone());

                                self.token_sources.push(TokenSource::ParseLexer(Lexer::new(
                                    self.str_interner.clone(),
                                    Some(loc),
                                    loc.pathref,
                                    reader,
                                )));
                                self.cwds.push(self.cwd.take().unwrap());
                                continue;
                            }

                            Some(Token::Directive {
                                loc,
                                name: DirectiveName::Each,
                            }) => {
                                let mut states = self.expect_each_directive(loc)?;
                                self.token_sources.push(self.token_source.take().unwrap());
                                self.cwds.push(self.cwd.clone().unwrap());

                                for _ in 0..states.len() {
                                    self.cwds.push(self.cwd.clone().unwrap());
                                }
                                self.cwd = None;
                                self.token_sources
                                    .extend(states.drain(..).rev().map(TokenSource::Macro));
                                continue;
                            }

                            Some(Token::Directive {
                                loc,
                                name: DirectiveName::Hex,
                            }) => {
                                let mac =
                                    self.expect_number_directive_arg(loc, DirectiveName::Hex, 16)?;

                                self.token_sources.push(self.token_source.take().unwrap());
                                self.cwds.push(self.cwd.unwrap().clone());

                                self.token_sources.push(TokenSource::Macro(mac));
                                self.cwds.push(self.cwd.take().unwrap());
                                continue;
                            }

                            Some(Token::Directive {
                                loc,
                                name: DirectiveName::Bin,
                            }) => {
                                let mac =
                                    self.expect_number_directive_arg(loc, DirectiveName::Bin, 2)?;

                                self.token_sources.push(self.token_source.take().unwrap());
                                self.cwds.push(self.cwd.unwrap().clone());

                                self.token_sources.push(TokenSource::Macro(mac));
                                self.cwds.push(self.cwd.take().unwrap());
                                continue;
                            }

                            Some(Token::Directive {
                                loc,
                                name: DirectiveName::IsDef,
                            }) => {
                                let direct = match self.next()? {
                                    None => return self.end_of_input_err(),

                                    Some(Token::Label { loc, value, kind }) => match kind {
                                        LabelKind::Global | LabelKind::Direct => value,

                                        LabelKind::Local => {
                                            if let Some(namespace) = self.active_namespace {
                                                let global_label = {
                                                    let interner =
                                                        self.str_interner.as_ref().borrow();
                                                    let global = interner.get(namespace).unwrap();
                                                    let label = interner.get(value).unwrap();
                                                    format!("{global}{label}")
                                                };
                                                self.str_interner.borrow_mut().intern(global_label)
                                            } else {
                                                let interner = self.str_interner.as_ref().borrow();
                                                let label = interner.get(value).unwrap();
                                                return asm_err!(loc, "The local symbol \"{label}\" is being read but there was no global label defined before it");
                                            }
                                        }
                                    },

                                    Some(tok) => {
                                        return asm_err!(
                                            tok.loc(),
                                            "Unexpected {}, expected a label",
                                            tok.as_display(&self.str_interner)
                                        )
                                    }
                                };
                                let value = if self.symtab.get(direct).is_some() {
                                    1
                                } else {
                                    0
                                };
                                self.stash = Some(Token::Number { loc, value });
                                self.loc = Some(loc);
                                continue;
                            }

                            _ => {}
                        }

                        self.stash = tok;
                        self.loc = Some(token_source.loc());
                        if self.stash.is_none() {
                            self.token_source = None;
                            self.cwd = None;
                        }
                    }
                },
            }
        }
    }

    fn next(&mut self) -> Result<Option<Token>, (SourceLoc, AssemblerError)> {
        if let Some(&_tok) = self.peek()? {
            // let depth = iter::repeat(' ')
            //     .take(self.token_sources.len())
            //     .collect::<String>();
            // println!("{}tok = {}", depth, tok.as_display(&self.str_interner));
        }
        Ok(self.stash.take())
    }

    fn trace_error(&self, loc: SourceLoc, e: AssemblerError) -> AssemblerError {
        let mut msg = String::new();
        let fmt_msg = &mut msg as &mut dyn Write;

        let path = self.file_manager.borrow().path(loc.pathref).unwrap();
        writeln!(fmt_msg, "In \"{}\"", path.display()).unwrap();

        if let Some(token_source) = self.token_source.as_ref() {
            let mut included_from = token_source.included_from();
            for token_source in self.token_sources.iter().rev() {
                // There are sources on the stack so included_from will be set
                let loc = included_from.unwrap();
                let path = self.file_manager.borrow().path(loc.pathref).unwrap();
                writeln!(
                    fmt_msg,
                    "\tIncluded from {}:{}:{}",
                    path.display(),
                    loc.line,
                    loc.column
                )
                .unwrap();
                included_from = token_source.included_from();
            }
        }
        writeln!(
            fmt_msg,
            "\n{}:{}:{}:",
            path.file_name().unwrap().to_str().unwrap(),
            loc.line,
            loc.column
        )
        .unwrap();
        writeln!(fmt_msg, "{e}").unwrap();
        AssemblerError(msg)
    }

    fn end_of_input_err<T>(&mut self) -> Result<T, (SourceLoc, AssemblerError)> {
        asm_err!(self.loc(), "Unexpected end of input")
    }

    #[inline]
    fn expect_symbol(&mut self, sym: SymbolName) -> Result<(), (SourceLoc, AssemblerError)> {
        match self.next()? {
            Some(Token::Symbol { loc, name }) => {
                if name != sym {
                    asm_err!(loc, "Unexpected symbol: \"{name}\", expected \"{sym}\"")
                } else {
                    Ok(())
                }
            }
            Some(tok) => asm_err!(
                tok.loc(),
                "Unexpected \"{}\", expected the symbol \"{sym}\"",
                tok.as_display(&self.str_interner)
            ),
            None => self.end_of_input_err(),
        }
    }

    #[inline]
    fn expect_register(&mut self, reg: RegisterName) -> Result<(), (SourceLoc, AssemblerError)> {
        match self.next()? {
            Some(Token::Register { loc, name }) => {
                if name != reg {
                    asm_err!(
                        loc,
                        "Unexpected register: \"{name}\", expected the register \"{reg}\""
                    )
                } else {
                    Ok(())
                }
            }
            Some(tok) => asm_err!(
                tok.loc(),
                "Unexpected {}, expected the register \"{reg}\"",
                tok.as_display(&self.str_interner)
            ),
            None => self.end_of_input_err(),
        }
    }

    fn expect_immediate(&mut self) -> Result<(), (SourceLoc, AssemblerError)> {
        let (loc, expr) = self.expr()?;
        if let Some(value) = expr.evaluate(&self.symtab) {
            if (value as u32) > (u8::MAX as u32) {
                return asm_err!(loc, "Expression result ({value}) will not fit in a byte");
            }
            self.data.push(value as u8);
        } else {
            self.links.push(Link::byte(loc, self.data.len(), expr));
            self.data.push(0);
        }
        Ok(())
    }

    fn expect_branch_immediate(&mut self) -> Result<(), (SourceLoc, AssemblerError)> {
        let (loc, mut expr) = self.expr()?;
        expr.push(ExprNode::Value(self.here.wrapping_add(2) as i32)); // subtract where the PC will be
        expr.push(ExprNode::Sub);
        if let Some(value) = expr.evaluate(&self.symtab) {
            if (value < (i8::MIN as i32)) || (value > (i8::MAX as i32)) {
                return asm_err!(loc, "Branch distance ({value}) will not fit in a byte");
            }
            self.data.push(value as u8);
        } else {
            self.links
                .push(Link::signed_byte(loc, self.data.len(), expr));
            self.data.push(0);
        }
        Ok(())
    }

    fn expect_macro_invoke(
        &mut self,
        name: StrRef,
    ) -> Result<MacroState, (SourceLoc, AssemblerError)> {
        let mut args = Vec::new();
        let arg_count = self.macros.get(&name).unwrap().args;
        let loc = self.loc();

        for i in 0..arg_count {
            let mut toks = Vec::new();

            let mut brace_depth = 0;
            loop {
                match self.next()? {
                    None => return self.end_of_input_err(),

                    Some(Token::NewLine { .. } | Token::Comment { .. }) => {}

                    Some(
                        tok @ Token::Symbol {
                            name: SymbolName::BraceOpen,
                            ..
                        },
                    ) => {
                        if brace_depth > 0 {
                            toks.push(tok);
                        }
                        brace_depth += 1;
                    }

                    Some(
                        tok @ Token::Symbol {
                            name: SymbolName::BraceClose,
                            ..
                        },
                    ) => {
                        brace_depth -= 1;
                        if brace_depth == 0 {
                            break;
                        }
                        toks.push(tok);
                    }

                    Some(tok) => {
                        toks.push(tok);
                        if brace_depth == 0 {
                            break;
                        }
                    }
                }
            }
            args.push(toks);
            if i < arg_count - 1 {
                self.expect_symbol(SymbolName::Comma)?;
            }
        }

        let uniq = self.uniq;
        self.uniq += 1;
        Ok(MacroState {
            name,
            args,
            macro_offset: 0,
            expanding_macro_arg: None,
            macro_arg_offset: 0,
            loc,
            uniq,
            included_from: Some(loc),
            str_interner: self.str_interner.clone(),
        })
    }

    fn expect_string_directive_arg(
        &mut self,
        loc: SourceLoc,
    ) -> Result<Token, (SourceLoc, AssemblerError)> {
        let mut string = String::new();
        let mut brace_depth = 0;
        loop {
            match self.next()? {
                None => return self.end_of_input_err(),

                Some(Token::NewLine { .. } | Token::Comment { .. }) => {}

                Some(Token::Symbol {
                    name: SymbolName::BraceOpen,
                    ..
                }) => {
                    if brace_depth > 0 {
                        string.push('{');
                    }
                    brace_depth += 1;
                }

                Some(Token::Symbol {
                    name: SymbolName::BraceClose,
                    ..
                }) => {
                    brace_depth -= 1;
                    if brace_depth == 0 {
                        break;
                    }
                    string.push('}');
                }

                Some(Token::String { value, .. }) => {
                    let interner = self.str_interner.as_ref().borrow();
                    let value = interner.get(value).unwrap();
                    string.push_str(value);
                }

                Some(Token::Label { value, .. }) => {
                    let interner = self.str_interner.as_ref().borrow();
                    let value = interner.get(value).unwrap();
                    string.push_str(value);
                }

                Some(Token::Number { value, .. }) => {
                    write!(string, "{value:x}").unwrap();
                }

                Some(Token::Operation { name, .. }) => {
                    write!(string, "{name}").unwrap();
                }

                Some(Token::Register { name, .. }) => {
                    write!(string, "{name}").unwrap();
                }

                Some(Token::Symbol { name, .. }) => {
                    write!(string, "{name}").unwrap();
                }

                Some(tok) => {
                    return asm_err!(
                        tok.loc(),
                        "Cannot stringify token: {}",
                        tok.as_display(&self.str_interner)
                    );
                }
            }
            if brace_depth == 0 {
                break;
            }
        }
        let value = self.str_interner.borrow_mut().intern(&string);
        Ok(Token::String { loc, value })
    }

    fn expect_number_directive_arg(
        &mut self,
        loc: SourceLoc,
        directive_name: DirectiveName,
        base: u32,
    ) -> Result<MacroState, (SourceLoc, AssemblerError)> {
        let value = match self.const_expr()? {
            (_, Some(value)) => {
                value
            }
            (loc, None) => {
                return asm_err!(
                    loc,
                    "The expression following an \"{directive_name}\" directive must be immediately solvable"
                )
            }
        };

        let mut interner = self.str_interner.borrow_mut();
        let name = interner.intern(format!("{directive_name} Invocation{}", self.uniq));
        self.uniq += 1;
        let value = match base {
            2 => interner.intern(format!("{value:b}")),
            16 => interner.intern(format!("{value:x}")),
            _ => unreachable!(),
        };
        let mut toks = Vec::new();
        toks.push(MacroToken::Token(Token::String { loc, value }));

        // There is a weird quirk with @hex / @bin
        // since it ends in a non-terminal (an expression)
        // there can be a token still in the stash..
        // This is a big problem because we need that token
        // to be placed after the tokens in this invocation
        if let Some(tok) = self.stash.take() {
            toks.push(MacroToken::Token(tok));
        }

        self.macros.insert(
            name,
            Macro {
                loc,
                args: 0,
                tokens: toks,
            },
        );

        let uniq = self.uniq;
        self.uniq += 1;
        Ok(MacroState {
            name,
            args: Vec::new(),
            macro_offset: 0,
            expanding_macro_arg: None,
            macro_arg_offset: 0,
            loc,
            uniq,
            included_from: Some(loc),
            str_interner: self.str_interner.clone(),
        })
    }

    fn expect_count_directive(
        &mut self,
        loc: SourceLoc,
    ) -> Result<MacroState, (SourceLoc, AssemblerError)> {
        let count = match self.const_expr()? {
            (loc, Some(value)) => {
                if value < 0 {
                    return asm_err!(
                        loc,
                        "\"@count\" expression result ({value}) must be positive"
                    );
                }
                value as usize
            }
            (loc, None) => {
                return asm_err!(
                    loc,
                    "The expression following an \"@count\" directive must be immediately solvable"
                )
            }
        };

        let name = self
            .str_interner
            .borrow_mut()
            .intern(format!("@count Invocation{}", self.uniq));
        self.uniq += 1;
        let mut toks = Vec::new();
        for i in 0..count {
            toks.push(MacroToken::Token(Token::Number {
                loc,
                value: i as u32,
            }));
        }

        // There is a weird quirk with @count
        // since it ends in a non-terminal (an expression)
        // there can be a token still in the stash..
        // This is a big problem because we need that token
        // to be placed after the tokens in this invocation
        if let Some(tok) = self.stash.take() {
            toks.push(MacroToken::Token(tok));
        }

        self.macros.insert(
            name,
            Macro {
                loc,
                args: 0,
                tokens: toks,
            },
        );

        let uniq = self.uniq;
        self.uniq += 1;
        Ok(MacroState {
            name,
            args: Vec::new(),
            macro_offset: 0,
            expanding_macro_arg: None,
            macro_arg_offset: 0,
            loc,
            uniq,
            included_from: Some(loc),
            str_interner: self.str_interner.clone(),
        })
    }

    fn expect_label_directive_arg(
        &mut self,
        loc: SourceLoc,
    ) -> Result<Token, (SourceLoc, AssemblerError)> {
        let mut string = String::new();
        let mut brace_depth = 0;
        loop {
            match self.next()? {
                None => return self.end_of_input_err(),

                Some(Token::NewLine { .. } | Token::Comment { .. }) => {}

                Some(Token::Symbol {
                    name: SymbolName::BraceOpen,
                    ..
                }) => {
                    if brace_depth > 0 {
                        string.push('{');
                    }
                    brace_depth += 1;
                }

                Some(Token::Symbol {
                    name: SymbolName::BraceClose,
                    ..
                }) => {
                    brace_depth -= 1;
                    if brace_depth == 0 {
                        break;
                    }
                    string.push('}');
                }

                Some(Token::String { value, .. }) => {
                    let interner = self.str_interner.as_ref().borrow();
                    let value = interner.get(value).unwrap();
                    string.push_str(value);
                }

                Some(Token::Label { value, .. }) => {
                    let interner = self.str_interner.as_ref().borrow();
                    let value = interner.get(value).unwrap();
                    string.push_str(value);
                }

                Some(Token::Number { value, .. }) => {
                    write!(string, "{value:x}").unwrap();
                }

                Some(Token::Operation { name, .. }) => {
                    write!(string, "{name}").unwrap();
                }

                Some(Token::Register { name, .. }) => {
                    write!(string, "{name}").unwrap();
                }

                Some(Token::Symbol { name, .. }) => {
                    write!(string, "{name}").unwrap();
                }

                Some(tok) => {
                    return asm_err!(
                        tok.loc(),
                        "Cannot labelify token: {}",
                        tok.as_display(&self.str_interner)
                    );
                }
            }
            if brace_depth == 0 {
                break;
            }
        }
        if string.split_whitespace().count() > 1 {
            return asm_err!(loc, "Malformed label: \"{string}\"");
        }
        let value = self.str_interner.borrow_mut().intern(&string);
        match string.chars().filter(|c| *c == '.').count() {
            0 => Ok(Token::Label {
                loc,
                kind: LabelKind::Global,
                value,
            }),
            1 => {
                if string.starts_with('.') {
                    Ok(Token::Label {
                        loc,
                        kind: LabelKind::Local,
                        value,
                    })
                } else {
                    Ok(Token::Label {
                        loc,
                        kind: LabelKind::Direct,
                        value,
                    })
                }
            }
            _ => asm_err!(loc, "Malformed label: \"{string}\""),
        }
    }

    fn expect_each_directive(
        &mut self,
        loc: SourceLoc,
    ) -> Result<Vec<MacroState>, (SourceLoc, AssemblerError)> {
        let mut args = Vec::new();
        let mut brace_depth = 0;
        loop {
            match self.next()? {
                None => return self.end_of_input_err(),

                Some(Token::NewLine { .. } | Token::Comment { .. }) => {}

                Some(
                    tok @ Token::Symbol {
                        name: SymbolName::BraceOpen,
                        ..
                    },
                ) => {
                    if brace_depth > 0 {
                        args.push(tok);
                    }
                    brace_depth += 1;
                }

                Some(
                    tok @ Token::Symbol {
                        name: SymbolName::BraceClose,
                        ..
                    },
                ) => {
                    brace_depth -= 1;
                    if brace_depth == 0 {
                        break;
                    }
                    args.push(tok);
                }

                Some(tok) => {
                    args.push(tok);
                }
            }
            if brace_depth == 0 {
                break;
            }
        }

        let name = self
            .str_interner
            .borrow_mut()
            .intern(format!("@each Invocation{}", self.uniq));
        self.uniq += 1;
        let mut toks = Vec::new();
        loop {
            match self.next()? {
                None => return self.end_of_input_err(),

                Some(Token::Directive {
                    name: DirectiveName::EndEach,
                    ..
                }) => {
                    break;
                }

                Some(Token::NestedDirective { loc, level, name }) => {
                    if level <= 2 {
                        toks.push(MacroToken::Token(Token::Directive { loc, name }));
                    } else {
                        toks.push(MacroToken::Token(Token::NestedDirective {
                            loc,
                            level: level - 1,
                            name,
                        }));
                    }
                }

                Some(Token::MacroArg { loc, value }) => {
                    if value == 0 {
                        toks.push(MacroToken::Uniq(loc));
                    } else {
                        toks.push(MacroToken::Argument {
                            loc,
                            index: value as usize - 1,
                        });
                    }
                }

                Some(Token::NestedMacroArg { loc, level, value }) => {
                    if level <= 2 {
                        toks.push(MacroToken::Token(Token::MacroArg { loc, value }));
                    } else {
                        toks.push(MacroToken::Token(Token::NestedMacroArg {
                            loc,
                            level: level - 1,
                            value,
                        }));
                    }
                }

                Some(tok) => {
                    toks.push(MacroToken::Token(tok));
                }
            }
        }

        self.macros.insert(
            name,
            Macro {
                loc,
                args: 1, // the current token
                tokens: toks,
            },
        );

        let mut states = Vec::new();
        for arg in args {
            let uniq = self.uniq;
            self.uniq += 1;
            states.push(MacroState {
                name,
                args: vec![vec![arg]],
                macro_offset: 0,
                expanding_macro_arg: None,
                macro_arg_offset: 0,
                loc,
                uniq,
                included_from: Some(loc),
                str_interner: self.str_interner.clone(),
            })
        }
        Ok(states)
    }

    fn const_expr(&mut self) -> Result<(SourceLoc, Option<i32>), (SourceLoc, AssemblerError)> {
        self.expr()
            .map(|(loc, expr)| (loc, expr.evaluate(&self.symtab)))
    }

    fn expr(&mut self) -> Result<(SourceLoc, Expr), (SourceLoc, AssemblerError)> {
        let mut nodes = Vec::new();
        let loc = self.expr_prec_0(&mut nodes)?;
        Ok((loc, Expr::new(nodes)))
    }

    fn expr_prec_0(
        &mut self,
        nodes: &mut Vec<ExprNode>,
    ) -> Result<SourceLoc, (SourceLoc, AssemblerError)> {
        let loc = self.expr_prec_1(nodes)?;

        match self.peek()? {
            Some(Token::Symbol {
                name: SymbolName::Question,
                ..
            }) => {
                self.next()?;
                self.expr_prec_1(nodes)?;
                if self.peeked_symbol(SymbolName::Colon)?.is_none() {
                    return asm_err!(self.loc(), "Expected a \":\" in ternary expression");
                }
                self.next()?;
                self.expr_prec_1(nodes)?;
                nodes.push(ExprNode::Ternary);
                Ok(loc)
            }
            _ => Ok(loc),
        }
    }

    fn expr_prec_1(
        &mut self,
        nodes: &mut Vec<ExprNode>,
    ) -> Result<SourceLoc, (SourceLoc, AssemblerError)> {
        let loc = self.expr_prec_2(nodes)?;

        loop {
            match self.peek()? {
                Some(Token::Symbol {
                    name: SymbolName::DoublePipe,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_2(nodes)?;
                    nodes.push(ExprNode::OrLogical);
                }
                _ => return Ok(loc),
            }
        }
    }

    fn expr_prec_2(
        &mut self,
        nodes: &mut Vec<ExprNode>,
    ) -> Result<SourceLoc, (SourceLoc, AssemblerError)> {
        let loc = self.expr_prec_3(nodes)?;

        loop {
            match self.peek()? {
                Some(Token::Symbol {
                    name: SymbolName::DoubleAmpersand,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_3(nodes)?;
                    nodes.push(ExprNode::AndLogical);
                }
                _ => return Ok(loc),
            }
        }
    }

    fn expr_prec_3(
        &mut self,
        nodes: &mut Vec<ExprNode>,
    ) -> Result<SourceLoc, (SourceLoc, AssemblerError)> {
        let loc = self.expr_prec_4(nodes)?;

        loop {
            match self.peek()? {
                Some(Token::Symbol {
                    name: SymbolName::Pipe,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_4(nodes)?;
                    nodes.push(ExprNode::Or);
                }
                _ => return Ok(loc),
            }
        }
    }

    fn expr_prec_4(
        &mut self,
        nodes: &mut Vec<ExprNode>,
    ) -> Result<SourceLoc, (SourceLoc, AssemblerError)> {
        let loc = self.expr_prec_5(nodes)?;

        loop {
            match self.peek()? {
                Some(Token::Symbol {
                    name: SymbolName::Caret,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_5(nodes)?;
                    nodes.push(ExprNode::Xor);
                }
                _ => return Ok(loc),
            }
        }
    }

    fn expr_prec_5(
        &mut self,
        nodes: &mut Vec<ExprNode>,
    ) -> Result<SourceLoc, (SourceLoc, AssemblerError)> {
        let loc = self.expr_prec_6(nodes)?;

        loop {
            match self.peek()? {
                Some(Token::Symbol {
                    name: SymbolName::Ampersand,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_6(nodes)?;
                    nodes.push(ExprNode::And);
                }
                _ => return Ok(loc),
            }
        }
    }

    fn expr_prec_6(
        &mut self,
        nodes: &mut Vec<ExprNode>,
    ) -> Result<SourceLoc, (SourceLoc, AssemblerError)> {
        let loc = self.expr_prec_7(nodes)?;

        loop {
            match self.peek()? {
                Some(Token::Symbol {
                    name: SymbolName::Equal,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_7(nodes)?;
                    nodes.push(ExprNode::Equal);
                }
                Some(Token::Symbol {
                    name: SymbolName::NotEqual,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_7(nodes)?;
                    nodes.push(ExprNode::NotEqual);
                }
                _ => return Ok(loc),
            }
        }
    }

    fn expr_prec_7(
        &mut self,
        nodes: &mut Vec<ExprNode>,
    ) -> Result<SourceLoc, (SourceLoc, AssemblerError)> {
        let loc = self.expr_prec_8(nodes)?;

        loop {
            match self.peek()? {
                Some(Token::Symbol {
                    name: SymbolName::LessThan,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_8(nodes)?;
                    nodes.push(ExprNode::LessThan);
                }
                Some(Token::Symbol {
                    name: SymbolName::LessEqual,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_8(nodes)?;
                    nodes.push(ExprNode::LessThanEqual);
                }
                Some(Token::Symbol {
                    name: SymbolName::GreaterThan,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_8(nodes)?;
                    nodes.push(ExprNode::GreaterThan);
                }
                Some(Token::Symbol {
                    name: SymbolName::GreaterEqual,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_8(nodes)?;
                    nodes.push(ExprNode::GreaterThanEqual);
                }
                _ => return Ok(loc),
            }
        }
    }

    fn expr_prec_8(
        &mut self,
        nodes: &mut Vec<ExprNode>,
    ) -> Result<SourceLoc, (SourceLoc, AssemblerError)> {
        let loc = self.expr_prec_9(nodes)?;

        loop {
            match self.peek()? {
                Some(Token::Symbol {
                    name: SymbolName::ShiftLeft,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_9(nodes)?;
                    nodes.push(ExprNode::ShiftLeft);
                }
                Some(Token::Symbol {
                    name: SymbolName::ShiftLeftLogical,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_9(nodes)?;
                    nodes.push(ExprNode::ShiftLeftLogical);
                }
                Some(Token::Symbol {
                    name: SymbolName::ShiftRight,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_9(nodes)?;
                    nodes.push(ExprNode::ShiftRight);
                }
                Some(Token::Symbol {
                    name: SymbolName::ShiftRightLogical,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_9(nodes)?;
                    nodes.push(ExprNode::ShiftRightLogical);
                }
                _ => return Ok(loc),
            }
        }
    }

    fn expr_prec_9(
        &mut self,
        nodes: &mut Vec<ExprNode>,
    ) -> Result<SourceLoc, (SourceLoc, AssemblerError)> {
        let loc = self.expr_prec_10(nodes)?;

        loop {
            match self.peek()? {
                Some(Token::Symbol {
                    name: SymbolName::Plus,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_10(nodes)?;
                    nodes.push(ExprNode::Add);
                }
                Some(Token::Symbol {
                    name: SymbolName::Minus,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_10(nodes)?;
                    nodes.push(ExprNode::Sub);
                }
                _ => return Ok(loc),
            }
        }
    }

    fn expr_prec_10(
        &mut self,
        nodes: &mut Vec<ExprNode>,
    ) -> Result<SourceLoc, (SourceLoc, AssemblerError)> {
        let loc = self.expr_prec_11(nodes)?;

        loop {
            match self.peek()? {
                Some(Token::Symbol {
                    name: SymbolName::Star,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_11(nodes)?;
                    nodes.push(ExprNode::Mul);
                }
                Some(Token::Symbol {
                    name: SymbolName::Div,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_11(nodes)?;
                    nodes.push(ExprNode::Div);
                }
                Some(Token::Symbol {
                    name: SymbolName::Mod,
                    ..
                }) => {
                    self.next()?;
                    self.expr_prec_11(nodes)?;
                    nodes.push(ExprNode::Rem);
                }
                _ => return Ok(loc),
            }
        }
    }

    fn expr_prec_11(
        &mut self,
        nodes: &mut Vec<ExprNode>,
    ) -> Result<SourceLoc, (SourceLoc, AssemblerError)> {
        match self.peek()? {
            Some(&Token::Symbol {
                loc,
                name: SymbolName::Minus,
            }) => {
                self.next()?;
                self.expr_prec_11(nodes)?;
                nodes.push(ExprNode::Neg);
                Ok(loc)
            }

            Some(&Token::Symbol {
                loc,
                name: SymbolName::Bang,
            }) => {
                self.next()?;
                self.expr_prec_11(nodes)?;
                nodes.push(ExprNode::NotLogical);
                Ok(loc)
            }

            Some(&Token::Symbol {
                loc,
                name: SymbolName::Tilde,
            }) => {
                self.next()?;
                self.expr_prec_11(nodes)?;
                nodes.push(ExprNode::Invert);
                Ok(loc)
            }

            Some(&Token::Symbol {
                loc,
                name: SymbolName::LessThan,
            }) => {
                self.next()?;
                self.expr_prec_11(nodes)?;
                nodes.push(ExprNode::Lo);
                Ok(loc)
            }

            Some(&Token::Symbol {
                loc,
                name: SymbolName::GreaterThan,
            }) => {
                self.next()?;
                self.expr_prec_11(nodes)?;
                nodes.push(ExprNode::Hi);
                Ok(loc)
            }

            Some(&Token::Symbol {
                loc,
                name: SymbolName::ParenOpen,
            }) => {
                self.next()?;
                self.expr_prec_0(nodes)?;
                if self.peeked_symbol(SymbolName::ParenClose)?.is_none() {
                    return asm_err!(self.loc(), "Expected a \")\" to close expression");
                }
                self.next()?;
                Ok(loc)
            }

            Some(&Token::Number { loc, value }) => {
                self.next()?;
                nodes.push(ExprNode::Value(value as i32));
                Ok(loc)
            }

            Some(&Token::Directive { loc, name }) => match name {
                DirectiveName::Here => {
                    self.next()?;
                    nodes.push(ExprNode::Value(self.here as i32));
                    Ok(loc)
                }
                _ => asm_err!(loc, "\"{name}\" directives are allowed in expressions"),
            },

            Some(&Token::Label { loc, kind, value }) => {
                self.next()?;
                let direct = match kind {
                    LabelKind::Global | LabelKind::Direct => value,

                    LabelKind::Local => {
                        if let Some(namespace) = self.active_namespace {
                            let direct_label = {
                                let interner = self.str_interner.as_ref().borrow();
                                let label = interner.get(value).unwrap();
                                let global = interner.get(namespace).unwrap();
                                format!("{global}{label}")
                            };
                            self.str_interner.borrow_mut().intern(direct_label)
                        } else {
                            let interner = self.str_interner.as_ref().borrow();
                            let label = interner.get(value).unwrap();
                            return asm_err!(loc, "The local label \"{label}\" is being defined but there was no global label defined before it");
                        }
                    }
                };

                if let Some(sym) = self.symtab.get(direct) {
                    match sym.inner() {
                        Symbol::Value(value) => {
                            nodes.push(ExprNode::Value(*value));
                        }
                        Symbol::Expr(expr) => {
                            if let Some(value) = expr.evaluate(&self.symtab) {
                                nodes.push(ExprNode::Value(value));
                            } else {
                                nodes.push(ExprNode::Label(direct));
                            }
                        }
                    }
                } else {
                    nodes.push(ExprNode::Label(direct));
                }
                // Important to record where in expressions we reference
                // symbols, so we can barf at link time
                self.symtab.touch(direct, loc);
                Ok(loc)
            }

            Some(&tok) => asm_err!(
                tok.loc(),
                "Unexpected {} in expression",
                tok.as_display(&self.str_interner)
            ),

            None => self.end_of_input_err(),
        }
    }

    #[inline]
    fn peeked_symbol(
        &mut self,
        sym: SymbolName,
    ) -> Result<Option<Token>, (SourceLoc, AssemblerError)> {
        match self.peek()? {
            Some(&tok @ Token::Symbol { name, .. }) if name == sym => Ok(Some(tok)),
            _ => Ok(None),
        }
    }

    fn parse_all(&mut self) -> Result<(), (SourceLoc, AssemblerError)> {
        loop {
            match self.peek()? {
                None => return Ok(()),

                // Note: see `peek` for details why we want to check for these!
                Some(Token::NewLine { .. } | Token::Comment { .. }) => {
                    self.next()?;
                }

                Some(&Token::Label { loc, value, kind }) => {
                    let direct = match kind {
                        LabelKind::Global => {
                            self.active_namespace = Some(value);
                            value
                        }

                        LabelKind::Direct => value,

                        LabelKind::Local => {
                            if let Some(namespace) = self.active_namespace {
                                let direct_label = {
                                    let interner = self.str_interner.as_ref().borrow();
                                    let label = interner.get(value).unwrap();
                                    let global = interner.get(namespace).unwrap();
                                    format!("{global}{label}")
                                };
                                self.str_interner.borrow_mut().intern(direct_label)
                            } else {
                                let interner = self.str_interner.as_ref().borrow();
                                let label = interner.get(value).unwrap();
                                return asm_err!(loc, "The local label \"{label}\" is being defined but there was no global label defined before it");
                            }
                        }
                    };

                    if self.symtab.get(direct).is_some() {
                        let interner = self.str_interner.as_ref().borrow();
                        let label = interner.get(direct).unwrap();
                        return asm_err!(loc, "The label \"{label}\" was already defined");
                    }
                    self.symtab.insert(direct, Symbol::Value(self.here as i32));
                    self.next()?;

                    if self.peeked_symbol(SymbolName::Colon)?.is_some() {
                        self.next()?;
                    }
                }

                Some(tok @ &Token::Directive { loc, name }) => {
                    match name {
                        DirectiveName::Org => {
                            self.next()?;

                            self.here = match self.const_expr()? {
                                (loc, Some(value)) => {
                                    if (value as u32) > (u16::MAX as u32) {
                                        return asm_err!(loc, "\"@org\" expression result ({value}) is not a valid address");
                                    }
                                    value as u32
                                },
                                (loc, None) => return asm_err!(loc, "The expression following an \"@org\" directive must be immediately solvable"),
                            };
                        }

                        DirectiveName::Echo => {
                            self.next()?;

                            match self.peek()? {
                                Some(&Token::String { value, ..  }) => {
                                    self.next()?;
                                    let interner = self.str_interner.as_ref().borrow();
                                    let value = interner.get(value).unwrap();
                                    eprintln!("{value}");
                                }

                                Some(_) => {
                                    match self.const_expr()? {
                                        (_, Some(value)) => {
                                            eprintln!("{value}");
                                        },
                                        (loc, None) => return asm_err!(loc, "An expression following an \"@echo\" directive must be immediately solvable"),
                                    }
                                }

                                None => return self.end_of_input_err()
                            }
                        }

                        DirectiveName::Die => {
                            self.next()?;

                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(&Token::String { value, .. }) => {
                                    self.next()?;
                                    let interner = self.str_interner.as_ref().borrow();
                                    let value = interner.get(value).unwrap();
                                    return asm_err!(loc, "{value}");
                                }

                                Some(_) => {
                                    match self.const_expr()? {
                                        (_, Some(value)) => return asm_err!(loc, "{value}"),
                                        (loc, None) => return asm_err!(loc, "An expression following an \"@die\" directive must be immediately solvable"),
                                    }
                                }
                            }
                        }

                        DirectiveName::Assert => {
                            self.next()?;

                            let (loc, expr) = self.expr()?;
                            let msg = if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                self.next()?;
                                match self.next()? {
                                    None => return self.end_of_input_err(),
                                    Some(Token::String { value, .. }) => Some(value),
                                    Some(tok) => {
                                        return asm_err!(
                                            tok.loc(),
                                            "Unexpected {}, expected a string",
                                            tok.as_display(&self.str_interner)
                                        );
                                    }
                                }
                            } else {
                                None
                            };

                            if let Some(value) = expr.evaluate(&self.symtab) {
                                if value == 0 {
                                    if let Some(msg) = msg {
                                        let interner = self.str_interner.as_ref().borrow();
                                        let msg = interner.get(msg).unwrap();
                                        return asm_err!(loc, "Assertion failed: {msg}",);
                                    } else {
                                        return asm_err!(loc, "Assertion failed");
                                    }
                                }
                            } else {
                                self.links.push(Link::assert(loc, msg, expr));
                            }
                        }

                        DirectiveName::Defl => {
                            self.next()?;

                            let (direct, loc) = match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(&Token::Label { loc, value, kind }) => match kind {
                                    LabelKind::Global | LabelKind::Direct => (value, loc),

                                    LabelKind::Local => {
                                        if let Some(namespace) = self.active_namespace {
                                            let global_label = {
                                                let interner = self.str_interner.as_ref().borrow();
                                                let global = interner.get(namespace).unwrap();
                                                let label = interner.get(value).unwrap();
                                                format!("{global}{label}")
                                            };
                                            (
                                                self.str_interner.borrow_mut().intern(global_label),
                                                loc,
                                            )
                                        } else {
                                            let interner = self.str_interner.as_ref().borrow();
                                            let label = interner.get(value).unwrap();
                                            return asm_err!(loc, "The local label \"{label}\" is being defined but there was no global label defined before it");
                                        }
                                    }
                                },
                                Some(tok) => {
                                    return asm_err!(tok.loc(), "A label name is required")
                                }
                            };
                            self.next()?;

                            if self.symtab.get(direct).is_some() {
                                let interner = self.str_interner.as_ref().borrow();
                                let label = interner.get(direct).unwrap();
                                return asm_err!(loc, "The label \"{label}\" was already defined");
                            }

                            self.expect_symbol(SymbolName::Comma)?;
                            let (_, expr) = self.expr()?;
                            self.symtab.insert(direct, Symbol::Expr(expr));
                        }

                        DirectiveName::Defn => {
                            self.next()?;

                            let (direct, loc) = match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(&Token::Label { loc, value, kind }) => match kind {
                                    LabelKind::Global | LabelKind::Direct => (value, loc),

                                    LabelKind::Local => {
                                        if let Some(namespace) = self.active_namespace {
                                            let global_label = {
                                                let interner = self.str_interner.as_ref().borrow();
                                                let global = interner.get(namespace).unwrap();
                                                let label = interner.get(value).unwrap();
                                                format!("{global}{label}")
                                            };
                                            (
                                                self.str_interner.borrow_mut().intern(global_label),
                                                loc,
                                            )
                                        } else {
                                            let interner = self.str_interner.as_ref().borrow();
                                            let label = interner.get(value).unwrap();
                                            return asm_err!(loc, "The local constant \"{label}\" is being defined but there was no global label defined before it");
                                        }
                                    }
                                },
                                Some(tok) => {
                                    return asm_err!(tok.loc(), "A constant name is required")
                                }
                            };
                            self.next()?;

                            if self.symtab.get(direct).is_some() {
                                let interner = self.str_interner.as_ref().borrow();
                                let label = interner.get(direct).unwrap();
                                return asm_err!(loc, "The constant \"{label}\" was already defined");
                            }

                            self.expect_symbol(SymbolName::Comma)?;
                            let (_, expr) = self.expr()?;
                            self.symtab.insert_no_meta(direct, Symbol::Expr(expr));
                        }

                        DirectiveName::ReDefl => {
                            self.next()?;

                            let direct = match self.next()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Label { loc, value, kind }) => match kind {
                                    LabelKind::Global | LabelKind::Direct => value,

                                    LabelKind::Local => {
                                        if let Some(namespace) = self.active_namespace {
                                            let global_label = {
                                                let interner = self.str_interner.as_ref().borrow();
                                                let global = interner.get(namespace).unwrap();
                                                let label = interner.get(value).unwrap();
                                                format!("{global}{label}")
                                            };
                                            self.str_interner.borrow_mut().intern(global_label)
                                        } else {
                                            let interner = self.str_interner.as_ref().borrow();
                                            let label = interner.get(value).unwrap();
                                            return asm_err!(loc, "The local label \"{label}\" is being defined but there was no global label defined before it");
                                        }
                                    }
                                },

                                Some(tok) => {
                                    return asm_err!(
                                        tok.loc(),
                                        "Unexpected {}, expected a label",
                                        tok.as_display(&self.str_interner)
                                    )
                                }
                            };

                            self.expect_symbol(SymbolName::Comma)?;
                            let (_, expr) = self.expr()?;
                            self.symtab.insert(direct, Symbol::Expr(expr));
                        }

                        DirectiveName::ReDefn => {
                            self.next()?;

                            let direct = match self.next()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Label { loc, value, kind }) => match kind {
                                    LabelKind::Global | LabelKind::Direct => value,

                                    LabelKind::Local => {
                                        if let Some(namespace) = self.active_namespace {
                                            let global_label = {
                                                let interner = self.str_interner.as_ref().borrow();
                                                let global = interner.get(namespace).unwrap();
                                                let label = interner.get(value).unwrap();
                                                format!("{global}{label}")
                                            };
                                            self.str_interner.borrow_mut().intern(global_label)
                                        } else {
                                            let interner = self.str_interner.as_ref().borrow();
                                            let label = interner.get(value).unwrap();
                                            return asm_err!(loc, "The local constant \"{label}\" is being defined but there was no global label defined before it");
                                        }
                                    }
                                },

                                Some(tok) => {
                                    return asm_err!(
                                        tok.loc(),
                                        "Unexpected {}, expected a label",
                                        tok.as_display(&self.str_interner)
                                    )
                                }
                            };

                            self.expect_symbol(SymbolName::Comma)?;
                            let (_, expr) = self.expr()?;
                            self.symtab.insert_no_meta(direct, Symbol::Expr(expr));
                        }

                        DirectiveName::UnDef => {
                            self.next()?;

                            let direct = match self.next()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Label { loc, value, kind }) => match kind {
                                    LabelKind::Global | LabelKind::Direct => value,

                                    LabelKind::Local => {
                                        if let Some(namespace) = self.active_namespace {
                                            let global_label = {
                                                let interner = self.str_interner.as_ref().borrow();
                                                let global = interner.get(namespace).unwrap();
                                                let label = interner.get(value).unwrap();
                                                format!("{global}{label}")
                                            };
                                            self.str_interner.borrow_mut().intern(global_label)
                                        } else {
                                            let interner = self.str_interner.as_ref().borrow();
                                            let label = interner.get(value).unwrap();
                                            return asm_err!(loc, "The local symbol \"{label}\" is being undefined but there was no global label defined before it");
                                        }
                                    }
                                },

                                Some(tok) => {
                                    return asm_err!(
                                        tok.loc(),
                                        "Unexpected {}, expected a label",
                                        tok.as_display(&self.str_interner)
                                    )
                                }
                            };
                            self.symtab.remove(direct);
                        }

                        DirectiveName::Db => {
                            self.next()?;

                            loop {
                                match self.peek()? {
                                    Some(&Token::String { loc, value, .. }) => {
                                        self.next()?;
                                        let interner = self.str_interner.as_ref().borrow();
                                        let bytes = interner.get(value).unwrap().as_bytes();

                                        if (self.here as usize) + bytes.len() > (u16::MAX as usize)
                                        {
                                            return asm_err!(
                                                loc,
                                                "\"@db\" bytes extend past address $ffff"
                                            );
                                        }
                                        self.here += bytes.len() as u32;
                                        self.data.extend_from_slice(bytes);
                                    }

                                    _ => {
                                        let (loc, expr) = self.expr()?;
                                        if let Some(value) = expr.evaluate(&self.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc, "\"@db\" expression result ({value}) will not fit in a byte");
                                            }
                                            if (self.here as usize) + 1 > (u16::MAX as usize) {
                                                return asm_err!(
                                                    loc,
                                                    "\"@db\" bytes extend past address $ffff"
                                                );
                                            }
                                            self.here += 1;
                                            self.data.push(value as u8);
                                        } else {
                                            self.here += 1;
                                            self.links.push(Link::byte(loc, self.data.len(), expr));
                                            self.data.push(0);
                                        }
                                    }
                                }

                                if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    self.next()?;
                                    continue;
                                }
                                break;
                            }
                        }

                        DirectiveName::Dw => {
                            self.next()?;

                            loop {
                                match self.peek()? {
                                    _ => {
                                        let (loc, expr) = self.expr()?;
                                        if let Some(value) = expr.evaluate(&self.symtab) {
                                            if (value as u32) > (u16::MAX as u32) {
                                                return asm_err!(
                                                    loc,
                                                    "\"@dw\" expression result ({value}) will not fit in a word"
                                                );
                                            }
                                            if (self.here as usize) + 1 > (u16::MAX as usize) {
                                                return asm_err!(
                                                    loc,
                                                    "\"@dw\" bytes extend past address $ffff"
                                                );
                                            }
                                            self.here += 2;
                                            self.data
                                                .extend_from_slice(&(value as u16).to_le_bytes());
                                        } else {
                                            self.here += 2;
                                            self.links.push(Link::word(loc, self.data.len(), expr));
                                            self.data.push(0);
                                            self.data.push(0);
                                        }
                                    }
                                }

                                if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    self.next()?;
                                    continue;
                                }
                                break;
                            }
                        }

                        DirectiveName::Ds => {
                            self.next()?;

                            let size = match self.const_expr()? {
                                (loc, None) => {
                                    return asm_err!(
                                    loc,
                                    "The size of a \"@ds\" directive must be immediately solvable"
                                );
                                }
                                (loc, Some(size)) => {
                                    if (size as u32) > (u16::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "\"@ds\" size expression result ({size}) will not fit in a word"
                                        );
                                    }
                                    if (self.here as usize) + (size as usize) > (u16::MAX as usize)
                                    {
                                        return asm_err!(
                                            loc,
                                            "\"@ds\" size extends past address $ffff"
                                        );
                                    }
                                    self.here += size as u32;
                                    size as usize
                                }
                            };

                            let value = if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                self.next()?;
                                let (loc, expr) = self.expr()?;
                                if let Some(value) = expr.evaluate(&self.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "\"@ds\" value expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    value as u8
                                } else {
                                    self.links
                                        .push(Link::space(loc, self.data.len(), size, expr));
                                    0
                                }
                            } else {
                                0
                            };
                            self.data.extend(iter::repeat(value).take(size));
                        }

                        DirectiveName::Include => {
                            self.next()?;
                            match self.next()? {
                                None => return self.end_of_input_err(),
                                Some(Token::String { loc, value }) => {
                                    let cwd = self
                                        .file_manager
                                        .path(self.cwd.unwrap())
                                        .unwrap()
                                        .to_path_buf();
                                    let interner = self.str_interner.as_ref().borrow();
                                    let path = interner.get(value).unwrap();
                                    let (pathref, reader) =
                                        match self.file_manager.reader(&cwd, path) {
                                            Ok(Some(tup)) => tup,
                                            Ok(None) => {
                                                return asm_err!(loc, "File not found: \"{path}\"");
                                            }
                                            Err(e) => {
                                                return asm_err!(
                                                    loc,
                                                    "Failed to open \"{path}\" for reading: {e}"
                                                );
                                            }
                                        };

                                    // Push old path
                                    self.token_sources.push(self.token_source.take().unwrap());
                                    self.cwds.push(self.cwd.take().unwrap());

                                    // Create new lexer and set cwd to file
                                    self.token_source = Some(TokenSource::Lexer(Lexer::new(
                                        self.str_interner.clone(),
                                        Some(loc),
                                        pathref,
                                        reader,
                                    )));
                                    let cwd = self
                                        .file_manager
                                        .path(pathref)
                                        .unwrap()
                                        .parent()
                                        .unwrap()
                                        .to_path_buf();
                                    self.cwd = Some(self.file_manager.intern(cwd, "."));
                                }
                                Some(tok) => {
                                    return asm_err!(
                                        tok.loc(),
                                        "Unexpected {}, expected file name string",
                                        tok.as_display(&self.str_interner)
                                    );
                                }
                            }
                        }

                        DirectiveName::Incbin => {
                            self.next()?;
                            match self.next()? {
                                None => return self.end_of_input_err(),
                                Some(Token::String { loc, value }) => {
                                    let cwd = self
                                        .file_manager
                                        .path(self.cwd.unwrap())
                                        .unwrap()
                                        .to_path_buf();
                                    let interner = self.str_interner.as_ref().borrow();
                                    let path = interner.get(value).unwrap();
                                    let (pathref, reader) =
                                        match self.file_manager.reader(&cwd, path) {
                                            Ok(Some(tup)) => tup,
                                            Ok(None) => {
                                                return asm_err!(loc, "File not found: \"{path}\"");
                                            }
                                            Err(e) => {
                                                return asm_err!(
                                                    loc,
                                                    "Failed to open \"{path}\" for reading: {e}"
                                                );
                                            }
                                        };

                                    let path = self.file_manager.path(pathref).unwrap();
                                    for result in reader.bytes() {
                                        match result {
                                            Ok(b) => {
                                                if (self.here as usize) + 1 > (u16::MAX as usize) {
                                                    return asm_err!(
                                                        loc,
                                                        "\"@incbin\" bytes extend past address $ffff"
                                                    );
                                                }
                                                self.here += 1;
                                                self.data.push(b);
                                            }
                                            Err(e) => {
                                                return asm_err!(
                                                    loc,
                                                    "Error reading \"{}\": {e}",
                                                    path.display()
                                                );
                                            }
                                        }
                                    }
                                }
                                Some(tok) => {
                                    return asm_err!(
                                        tok.loc(),
                                        "Unexpected {}, expected file name string",
                                        tok.as_display(&self.str_interner)
                                    );
                                }
                            }
                        }

                        DirectiveName::Macro => {
                            self.next()?;

                            let macro_loc = loc;
                            let (value, loc) = match self.next()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Label {
                                    loc,
                                    kind: LabelKind::Global,
                                    value,
                                }) => (value, loc),

                                Some(tok) => {
                                    return asm_err!(
                                        tok.loc(),
                                        "Unexpected {}, expected a macro name",
                                        tok.as_display(&self.str_interner)
                                    );
                                }
                            };
                            if self.macros.contains_key(&value) {
                                let interner = self.str_interner.as_ref().borrow();
                                let name = interner.get(value).unwrap();
                                return asm_err!(loc, "The macro \"{name}\" was already defined");
                            }

                            self.expect_symbol(SymbolName::Comma)?;

                            let args = match self.next()? {
                                None => return self.end_of_input_err(),
                                Some(Token::Number { value, .. }) => value as usize,
                                Some(tok) => {
                                    return asm_err!(
                                        tok.loc(),
                                        "Unexpected {}, expected number of macro arguments",
                                        tok.as_display(&self.str_interner)
                                    );
                                }
                            };

                            self.active_macro = Some(value);
                            let mut toks = Vec::new();
                            loop {
                                match self.next()? {
                                    None => return self.end_of_input_err(),
                                    Some(Token::Comment { .. } | Token::NewLine { .. }) => {}

                                    Some(Token::Directive {
                                        name: DirectiveName::EndMacro,
                                        ..
                                    }) => {
                                        self.active_macro = None;
                                        break;
                                    }

                                    Some(Token::NestedDirective { loc, level, name }) => {
                                        if level <= 2 {
                                            toks.push(MacroToken::Token(Token::Directive {
                                                loc,
                                                name,
                                            }));
                                        } else {
                                            toks.push(MacroToken::Token(Token::NestedDirective {
                                                loc,
                                                level: level - 1,
                                                name,
                                            }));
                                        }
                                    }

                                    Some(Token::MacroArg { loc, value }) => {
                                        if value == 0 {
                                            toks.push(MacroToken::Uniq(loc));
                                        } else {
                                            toks.push(MacroToken::Argument {
                                                loc,
                                                index: value as usize - 1,
                                            });
                                        }
                                    }

                                    Some(Token::NestedMacroArg { loc, level, value }) => {
                                        if level <= 2 {
                                            toks.push(MacroToken::Token(Token::MacroArg {
                                                loc,
                                                value,
                                            }));
                                        } else {
                                            toks.push(MacroToken::Token(Token::NestedMacroArg {
                                                loc,
                                                level: level - 1,
                                                value,
                                            }));
                                        }
                                    }

                                    Some(tok) => {
                                        toks.push(MacroToken::Token(tok));
                                    }
                                }
                            }

                            self.macros.insert(
                                value,
                                Macro {
                                    loc: macro_loc,
                                    args,
                                    tokens: toks,
                                },
                            );
                        }

                        DirectiveName::Struct => {
                            self.next()?;

                            let (value, loc) = match self.next()? {
                                None => return self.end_of_input_err(),
                                Some(Token::Label {
                                    loc,
                                    kind: LabelKind::Global,
                                    value,
                                }) => (value, loc),
                                Some(tok) => {
                                    return asm_err!(
                                        tok.loc(),
                                        "Unexpected {}, expected struct name",
                                        tok.as_display(&self.str_interner)
                                    );
                                }
                            };

                            let old_namespace = self.active_namespace;
                            if self.symtab.get(value).is_some() {
                                let interner = self.str_interner.as_ref().borrow();
                                let label = interner.get(value).unwrap();
                                return asm_err!(loc, "The label \"{label}\" was already defined");
                            }
                            self.active_namespace = Some(value);
                            let mut struct_size = 0i32;
                            loop {
                                match self.next()? {
                                    None => return self.end_of_input_err(),
                                    Some(Token::NewLine { .. } | Token::Comment { .. }) => {}
                                    Some(Token::Directive {
                                        name: DirectiveName::Ds,
                                        ..
                                    }) => match self.peek()? {
                                        None => return self.end_of_input_err(),
                                        Some(_) => match self.const_expr()? {
                                            (loc, None) => {
                                                return asm_err!(
                                                    loc,
                                                    "Padding size must be immediately solvable",
                                                );
                                            }
                                            (_, Some(pad_size)) => {
                                                struct_size = struct_size.wrapping_add(pad_size);
                                            }
                                        },
                                    },
                                    Some(Token::Directive {
                                        name: DirectiveName::Align,
                                        ..
                                    }) => match self.peek()? {
                                        None => return self.end_of_input_err(),
                                        Some(_) => match self.const_expr()? {
                                            (loc, None) => {
                                                return asm_err!(
                                                    loc,
                                                    "Alignment must be immediately solvable",
                                                );
                                            }
                                            (_, Some(alignment)) => {
                                                if alignment < 2 {
                                                    return asm_err!(
                                                                loc,
                                                                "Alignment value ({alignment}) must be greater than 1",
                                                            );
                                                }
                                                let padding = (alignment
                                                    - (struct_size % alignment))
                                                    % alignment;
                                                struct_size = struct_size.wrapping_add(padding);
                                            }
                                        },
                                    },
                                    Some(Token::Directive {
                                        name: DirectiveName::EndStruct,
                                        ..
                                    }) => break,
                                    Some(Token::Label {
                                        loc,
                                        kind: LabelKind::Global,
                                        value: field,
                                    }) => {
                                        let direct_label = {
                                            let interner = self.str_interner.as_ref().borrow();
                                            let global = interner.get(value).unwrap();
                                            let label = interner.get(field).unwrap();
                                            format!("{global}.{label}")
                                        };
                                        let direct =
                                            self.str_interner.borrow_mut().intern(&direct_label);
                                        if self.symtab.get(direct).is_some() {
                                            return asm_err!(
                                                loc,
                                                "The field \"{direct_label}\" was already defined",
                                            );
                                        }
                                        match self.peek()? {
                                            None => return self.end_of_input_err(),
                                            Some(_) => match self.const_expr()? {
                                                (loc, None) => {
                                                    return asm_err!(
                                                                loc,
                                                                "Field \"{direct_label}\"'s size must be immediately solvable",
                                                            );
                                                }
                                                (_, Some(field_size)) => {
                                                    self.symtab
                                                        .insert_no_meta(direct, Symbol::Value(struct_size));
                                                    struct_size =
                                                        struct_size.wrapping_add(field_size);
                                                }
                                            },
                                        }
                                    }
                                    Some(tok) => {
                                        return asm_err!(
                                            tok.loc(),
                                            "Unexpected {}, expected field name or \"@ends\"",
                                            tok.as_display(&self.str_interner)
                                        );
                                    }
                                }
                            }
                            self.active_namespace = old_namespace;
                            self.symtab.insert_no_meta(value, Symbol::Value(struct_size));
                        }

                        DirectiveName::Align => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),
                                Some(_) => match self.const_expr()? {
                                    (loc, None) => {
                                        return asm_err!(
                                            loc,
                                            "Alignment must be immediately solvable"
                                        );
                                    }
                                    (loc, Some(alignment)) => {
                                        if alignment < 2 {
                                            return asm_err!(
                                                loc,
                                                "Alignment value ({alignment}) must be greater than 1",
                                            );
                                        }
                                        let alignment = alignment as u32;
                                        let padding =
                                            (alignment - (self.here % alignment)) % alignment;
                                        if padding > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Alignment padding ({padding}) will not fit in a word"
                                            );
                                        }
                                        if (self.here as usize) + (padding as usize)
                                            > (u16::MAX as usize)
                                        {
                                            return asm_err!(
                                                loc,
                                                "Alignment padding extends past address $ffff"
                                            );
                                        }
                                        self.here += padding;
                                        self.data.extend(iter::repeat(0).take(padding as usize));
                                    }
                                },
                            }
                        }

                        DirectiveName::Meta => {
                            self.next()?;

                            let mut pairs = Vec::new();
                            loop {
                                match self.next()? {
                                    None => return self.end_of_input_err(),

                                    Some(Token::String { value: key, .. }) => match self.next()? {
                                        None => return self.end_of_input_err(),

                                        Some(Token::String { value, .. }) => {
                                            pairs.push([key, value]);
                                        }

                                        Some(tok) => {
                                            return asm_err!(
                                                tok.loc(),
                                                "Unexpected {}, expected a metadata value",
                                                tok.clone().as_display(&self.str_interner)
                                            );
                                        }
                                    },

                                    Some(tok) => {
                                        return asm_err!(
                                            tok.loc(),
                                            "Unexpected {}, expected a metadata key",
                                            tok.clone().as_display(&self.str_interner)
                                        );
                                    }
                                }

                                if self.peeked_symbol(SymbolName::Comma)?.is_none() {
                                    break;
                                }
                                self.next()?;
                            }
                            self.symtab.set_meta(pairs);
                        }

                        DirectiveName::EndMeta => {
                            self.next()?;
                            self.symtab.set_meta(&[]);
                        }

                        _ => {
                            return asm_err!(
                                tok.loc(),
                                "Unexpected {}",
                                tok.clone().as_display(&self.str_interner)
                            );
                        }
                    }
                }

                Some(Token::Operation { name, .. }) => {
                    match name {
                        OperationName::Adc => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Symbol {
                                    name: SymbolName::Hash,
                                    ..
                                }) => {
                                    self.next()?;
                                    self.data.push(0x69);
                                    self.expect_immediate()?;
                                    self.here += 2;
                                }

                                Some(Token::Symbol {
                                    name: SymbolName::ParenOpen,
                                    ..
                                }) => {
                                    self.next()?;
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) > (u8::MAX as u32) {
                                            return asm_err!(loc, "Expression result ({value}) will not fit in a byte");
                                        }
                                        value as u8
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::byte(loc, self.data.len() + 1, expr));
                                        0
                                    };
                                    match self.next()? {
                                        None => return self.end_of_input_err(),

                                        Some(Token::Symbol {
                                            name: SymbolName::Comma,
                                            ..
                                        }) => {
                                            self.data.push(0x61);
                                            self.expect_register(RegisterName::X)?;
                                            self.expect_symbol(SymbolName::ParenClose)?;
                                        }

                                        Some(Token::Symbol {
                                            name: SymbolName::ParenClose,
                                            ..
                                        }) => {
                                            self.data.push(0x71);
                                            self.expect_symbol(SymbolName::Comma)?;
                                            self.expect_register(RegisterName::Y)?;
                                        }

                                        Some(tok) => {
                                            return asm_err!(
                                                tok.loc(),
                                                "Unexpected {}, expected \",\" or \")\"",
                                                tok.as_display(&self.str_interner)
                                            );
                                        }
                                    }
                                    self.data.push(value);
                                    self.here += 2;
                                }

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                                self.next()?;
                                                self.expect_register(RegisterName::X)?;
                                                self.data.push(0x75);
                                            } else {
                                                self.data.push(0x65);
                                            }
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                        self.next()?;
                                        match self.next()? {
                                            None => return self.end_of_input_err(),

                                            Some(Token::Register {
                                                name: RegisterName::X,
                                                ..
                                            }) => {
                                                self.data.push(0x7D);
                                            }

                                            Some(Token::Register {
                                                name: RegisterName::Y,
                                                ..
                                            }) => {
                                                self.data.push(0x79);
                                            }

                                            Some(tok) => {
                                                return asm_err!(
                                                    tok.loc(),
                                                    "Unexpected {}, expected register \"x\" or \"y\"",
                                                    tok.as_display(&self.str_interner)
                                                );
                                            }
                                        }
                                    } else {
                                        self.data.push(0x6D);
                                    }
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::And => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Symbol {
                                    name: SymbolName::Hash,
                                    ..
                                }) => {
                                    self.next()?;
                                    self.data.push(0x29);
                                    self.expect_immediate()?;
                                    self.here += 2;
                                }

                                Some(Token::Symbol {
                                    name: SymbolName::ParenOpen,
                                    ..
                                }) => {
                                    self.next()?;
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) > (u8::MAX as u32) {
                                            return asm_err!(loc, "Expression result ({value}) will not fit in a byte");
                                        }
                                        value as u8
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::byte(loc, self.data.len() + 1, expr));
                                        0
                                    };
                                    match self.next()? {
                                        None => return self.end_of_input_err(),

                                        Some(Token::Symbol {
                                            name: SymbolName::Comma,
                                            ..
                                        }) => {
                                            self.data.push(0x21);
                                            self.expect_register(RegisterName::X)?;
                                            self.expect_symbol(SymbolName::ParenClose)?;
                                        }

                                        Some(Token::Symbol {
                                            name: SymbolName::ParenClose,
                                            ..
                                        }) => {
                                            self.data.push(0x31);
                                            self.expect_symbol(SymbolName::Comma)?;
                                            self.expect_register(RegisterName::Y)?;
                                        }

                                        Some(tok) => {
                                            return asm_err!(
                                                tok.loc(),
                                                "Unexpected {}, expected \",\" or \")\"",
                                                tok.as_display(&self.str_interner)
                                            );
                                        }
                                    }
                                    self.here += 2;
                                    self.data.push(value);
                                }

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                                self.next()?;
                                                self.expect_register(RegisterName::X)?;
                                                self.data.push(0x35);
                                            } else {
                                                self.data.push(0x25);
                                            }
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                        self.next()?;
                                        match self.next()? {
                                            None => return self.end_of_input_err(),

                                            Some(Token::Register {
                                                name: RegisterName::X,
                                                ..
                                            }) => {
                                                self.data.push(0x3D);
                                            }

                                            Some(Token::Register {
                                                name: RegisterName::Y,
                                                ..
                                            }) => {
                                                self.data.push(0x39);
                                            }

                                            Some(tok) => {
                                                return asm_err!(
                                                    tok.loc(),
                                                    "Unexpected {}, expected register \"x\" or \"y\"",
                                                    tok.as_display(&self.str_interner)
                                                );
                                            }
                                        }
                                    } else {
                                        self.data.push(0x2D);
                                    }
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Asl => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Register {
                                    name: RegisterName::A,
                                    ..
                                }) => {
                                    self.next()?;
                                    self.here += 1;
                                    self.data.push(0x0A);
                                }

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                                self.next()?;
                                                self.expect_register(RegisterName::X)?;
                                                self.data.push(0x16);
                                            } else {
                                                self.data.push(0x06);
                                            }
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                        self.next()?;
                                        self.expect_register(RegisterName::X)?;
                                        self.data.push(0x1E);
                                    } else {
                                        self.data.push(0x0E);
                                    }
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Bcc => {
                            self.next()?;
                            self.data.push(0x90);
                            self.expect_branch_immediate()?;
                            self.here += 2;
                        }

                        OperationName::Bcs => {
                            self.next()?;
                            self.data.push(0xB0);
                            self.expect_branch_immediate()?;
                            self.here += 2;
                        }

                        OperationName::Beq => {
                            self.next()?;
                            self.data.push(0xF0);
                            self.expect_branch_immediate()?;
                            self.here += 2;
                        }

                        OperationName::Bit => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            self.data.push(0x24);
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    self.data.push(0x2C);
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Bmi => {
                            self.next()?;
                            self.data.push(0x30);
                            self.expect_branch_immediate()?;
                            self.here += 2;
                        }

                        OperationName::Bne => {
                            self.next()?;
                            self.data.push(0xD0);
                            self.expect_branch_immediate()?;
                            self.here += 2;
                        }

                        OperationName::Bpl => {
                            self.next()?;
                            self.data.push(0x10);
                            self.expect_branch_immediate()?;
                            self.here += 2;
                        }

                        OperationName::Brk => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0x00);
                        }

                        OperationName::Bvc => {
                            self.next()?;
                            self.data.push(0x50);
                            self.expect_branch_immediate()?;
                            self.here += 2;
                        }

                        OperationName::Bvs => {
                            self.next()?;
                            self.data.push(0x70);
                            self.expect_branch_immediate()?;
                            self.here += 2;
                        }

                        OperationName::Clc => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0x18);
                        }

                        OperationName::Cld => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0xD8);
                        }

                        OperationName::Cli => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0x58);
                        }

                        OperationName::Clv => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0xB8);
                        }

                        OperationName::Cmp => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Symbol {
                                    name: SymbolName::Hash,
                                    ..
                                }) => {
                                    self.next()?;
                                    self.data.push(0xC9);
                                    self.expect_immediate()?;
                                    self.here += 2;
                                }

                                Some(Token::Symbol {
                                    name: SymbolName::ParenOpen,
                                    ..
                                }) => {
                                    self.next()?;
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) > (u8::MAX as u32) {
                                            return asm_err!(loc, "Expression result ({value}) will not fit in a byte");
                                        }
                                        value as u8
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::byte(loc, self.data.len() + 1, expr));
                                        0
                                    };
                                    match self.next()? {
                                        None => return self.end_of_input_err(),

                                        Some(Token::Symbol {
                                            name: SymbolName::Comma,
                                            ..
                                        }) => {
                                            self.data.push(0xC1);
                                            self.expect_register(RegisterName::X)?;
                                            self.expect_symbol(SymbolName::ParenClose)?;
                                        }

                                        Some(Token::Symbol {
                                            name: SymbolName::ParenClose,
                                            ..
                                        }) => {
                                            self.data.push(0xD1);
                                            self.expect_symbol(SymbolName::Comma)?;
                                            self.expect_register(RegisterName::Y)?;
                                        }

                                        Some(tok) => {
                                            return asm_err!(
                                                tok.loc(),
                                                "Unexpected {}, expected \",\" or \")\"",
                                                tok.as_display(&self.str_interner)
                                            );
                                        }
                                    }
                                    self.here += 2;
                                    self.data.push(value);
                                }

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                                self.next()?;
                                                self.expect_register(RegisterName::X)?;
                                                self.data.push(0xD5);
                                            } else {
                                                self.data.push(0xC5);
                                            }
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                        self.next()?;
                                        match self.next()? {
                                            None => return self.end_of_input_err(),

                                            Some(Token::Register {
                                                name: RegisterName::X,
                                                ..
                                            }) => {
                                                self.data.push(0xDD);
                                            }

                                            Some(Token::Register {
                                                name: RegisterName::Y,
                                                ..
                                            }) => {
                                                self.data.push(0xD9);
                                            }

                                            Some(tok) => {
                                                return asm_err!(
                                                    tok.loc(),
                                                    "Unexpected {}, expected register \"x\" or \"y\"",
                                                    tok.as_display(&self.str_interner)
                                                );
                                            }
                                        }
                                    } else {
                                        self.data.push(0xCD);
                                    }
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Cpx => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Symbol {
                                    name: SymbolName::Hash,
                                    ..
                                }) => {
                                    self.next()?;
                                    self.data.push(0xE0);
                                    self.expect_immediate()?;
                                    self.here += 2;
                                }

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            self.data.push(0xE4);
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    self.data.push(0xEC);
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Cpy => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Symbol {
                                    name: SymbolName::Hash,
                                    ..
                                }) => {
                                    self.next()?;
                                    self.data.push(0xC0);
                                    self.expect_immediate()?;
                                    self.here += 2;
                                }

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            self.data.push(0xC4);
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    self.data.push(0xCC);
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Dec => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                                self.next()?;
                                                self.expect_register(RegisterName::X)?;
                                                self.data.push(0xD6);
                                            } else {
                                                self.data.push(0xC6);
                                            }
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                        self.next()?;
                                        self.expect_register(RegisterName::X)?;
                                        self.data.push(0xDE);
                                    } else {
                                        self.data.push(0xCE);
                                    }
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Dex => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0xCA);
                        }

                        OperationName::Dey => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0x88);
                        }

                        OperationName::Eor => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Symbol {
                                    name: SymbolName::Hash,
                                    ..
                                }) => {
                                    self.next()?;
                                    self.data.push(0x49);
                                    self.expect_immediate()?;
                                    self.here += 2;
                                }

                                Some(Token::Symbol {
                                    name: SymbolName::ParenOpen,
                                    ..
                                }) => {
                                    self.next()?;
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) > (u8::MAX as u32) {
                                            return asm_err!(loc, "Expression result ({value}) will not fit in a byte");
                                        }
                                        value as u8
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::byte(loc, self.data.len() + 1, expr));
                                        0
                                    };
                                    match self.next()? {
                                        None => return self.end_of_input_err(),

                                        Some(Token::Symbol {
                                            name: SymbolName::Comma,
                                            ..
                                        }) => {
                                            self.data.push(0x41);
                                            self.expect_register(RegisterName::X)?;
                                            self.expect_symbol(SymbolName::ParenClose)?;
                                        }

                                        Some(Token::Symbol {
                                            name: SymbolName::ParenClose,
                                            ..
                                        }) => {
                                            self.data.push(0x51);
                                            self.expect_symbol(SymbolName::Comma)?;
                                            self.expect_register(RegisterName::Y)?;
                                        }

                                        Some(tok) => {
                                            return asm_err!(
                                                tok.loc(),
                                                "Unexpected {}, expected \",\" or \")\"",
                                                tok.as_display(&self.str_interner)
                                            );
                                        }
                                    }
                                    self.here += 2;
                                    self.data.push(value);
                                }

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                                self.next()?;
                                                self.expect_register(RegisterName::X)?;
                                                self.data.push(0x55);
                                            } else {
                                                self.data.push(0x45);
                                            }
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                        self.next()?;
                                        match self.next()? {
                                            None => return self.end_of_input_err(),

                                            Some(Token::Register {
                                                name: RegisterName::X,
                                                ..
                                            }) => {
                                                self.data.push(0x5D);
                                            }

                                            Some(Token::Register {
                                                name: RegisterName::Y,
                                                ..
                                            }) => {
                                                self.data.push(0x59);
                                            }

                                            Some(tok) => {
                                                return asm_err!(
                                                    tok.loc(),
                                                    "Unexpected {}, expected register \"x\" or \"y\"",
                                                    tok.as_display(&self.str_interner)
                                                );
                                            }
                                        }
                                    } else {
                                        self.data.push(0x4D);
                                    }
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Inc => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                                self.next()?;
                                                self.expect_register(RegisterName::X)?;
                                                self.data.push(0xF6);
                                            } else {
                                                self.data.push(0xE6);
                                            }
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                        self.next()?;
                                        self.expect_register(RegisterName::X)?;
                                        self.data.push(0xFE);
                                    } else {
                                        self.data.push(0xEE);
                                    }
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Inx => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0xE8);
                        }

                        OperationName::Iny => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0xC8);
                        }

                        OperationName::Jmp => {
                            self.next()?;

                            let need_paren = if self.peeked_symbol(SymbolName::ParenOpen)?.is_some()
                            {
                                self.next()?;
                                self.data.push(0x6C);
                                true
                            } else {
                                self.data.push(0x4C);
                                false
                            };

                            let (loc, expr) = self.expr()?;
                            if let Some(value) = expr.evaluate(&self.symtab) {
                                if (value as u32) > (u16::MAX as u32) {
                                    return asm_err!(
                                        loc,
                                        "Expression result ({value}) will not fit in a word"
                                    );
                                }
                                self.data.extend_from_slice(&(value as u16).to_le_bytes());
                            } else {
                                self.links.push(Link::word(loc, self.data.len(), expr));
                                self.data.push(0);
                                self.data.push(0);
                            };

                            self.here += 3;
                            if need_paren {
                                self.expect_symbol(SymbolName::ParenClose)?;
                            }
                        }

                        OperationName::Jsr => {
                            self.next()?;
                            self.data.push(0x20);
                            let (loc, expr) = self.expr()?;
                            if let Some(value) = expr.evaluate(&self.symtab) {
                                if (value as u32) > (u16::MAX as u32) {
                                    return asm_err!(
                                        loc,
                                        "Expression result ({value}) will not fit in a word"
                                    );
                                }
                                self.data.extend_from_slice(&(value as u16).to_le_bytes());
                            } else {
                                self.links.push(Link::word(loc, self.data.len(), expr));
                                self.data.push(0);
                                self.data.push(0);
                            };
                            self.here += 3;
                        }

                        OperationName::Lda => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Symbol {
                                    name: SymbolName::Hash,
                                    ..
                                }) => {
                                    self.next()?;
                                    self.data.push(0xA9);
                                    self.expect_immediate()?;
                                    self.here += 2;
                                }

                                Some(Token::Symbol {
                                    name: SymbolName::ParenOpen,
                                    ..
                                }) => {
                                    self.next()?;
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) > (u8::MAX as u32) {
                                            return asm_err!(loc, "Expression result ({value}) will not fit in a byte");
                                        }
                                        value as u8
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::byte(loc, self.data.len() + 1, expr));
                                        0
                                    };
                                    match self.next()? {
                                        None => return self.end_of_input_err(),

                                        Some(Token::Symbol {
                                            name: SymbolName::Comma,
                                            ..
                                        }) => {
                                            self.data.push(0xA1);
                                            self.expect_register(RegisterName::X)?;
                                            self.expect_symbol(SymbolName::ParenClose)?;
                                        }

                                        Some(Token::Symbol {
                                            name: SymbolName::ParenClose,
                                            ..
                                        }) => {
                                            self.data.push(0xB1);
                                            self.expect_symbol(SymbolName::Comma)?;
                                            self.expect_register(RegisterName::Y)?;
                                        }

                                        Some(tok) => {
                                            return asm_err!(
                                                tok.loc(),
                                                "Unexpected {}, expected \",\" or \")\"",
                                                tok.as_display(&self.str_interner)
                                            );
                                        }
                                    }
                                    self.here += 2;
                                    self.data.push(value);
                                }

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                                self.next()?;
                                                self.expect_register(RegisterName::X)?;
                                                self.data.push(0xB5);
                                            } else {
                                                self.data.push(0xA5);
                                            }
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                        self.next()?;
                                        match self.next()? {
                                            None => return self.end_of_input_err(),

                                            Some(Token::Register {
                                                name: RegisterName::X,
                                                ..
                                            }) => {
                                                self.data.push(0xBD);
                                            }

                                            Some(Token::Register {
                                                name: RegisterName::Y,
                                                ..
                                            }) => {
                                                self.data.push(0xB9);
                                            }

                                            Some(tok) => {
                                                return asm_err!(
                                                    tok.loc(),
                                                    "Unexpected {}, expected register \"x\" or \"y\"",
                                                    tok.as_display(&self.str_interner)
                                                );
                                            }
                                        }
                                    } else {
                                        self.data.push(0xAD);
                                    }
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Ldx => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Symbol {
                                    name: SymbolName::Hash,
                                    ..
                                }) => {
                                    self.next()?;
                                    self.data.push(0xA2);
                                    self.expect_immediate()?;
                                    self.here += 2;
                                }

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                                self.next()?;
                                                self.expect_register(RegisterName::Y)?;
                                                self.data.push(0xB6);
                                            } else {
                                                self.data.push(0xA6);
                                            }
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                        self.next()?;
                                        self.expect_register(RegisterName::Y)?;
                                        self.data.push(0xBE);
                                    } else {
                                        self.data.push(0xAE);
                                    }
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Ldy => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Symbol {
                                    name: SymbolName::Hash,
                                    ..
                                }) => {
                                    self.next()?;
                                    self.data.push(0xA0);
                                    self.expect_immediate()?;
                                    self.here += 2;
                                }

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                                self.next()?;
                                                self.expect_register(RegisterName::X)?;
                                                self.data.push(0xB4);
                                            } else {
                                                self.data.push(0xA4);
                                            }
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                        self.next()?;
                                        self.expect_register(RegisterName::X)?;
                                        self.data.push(0xBC);
                                    } else {
                                        self.data.push(0xAC);
                                    }
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Lsr => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Register {
                                    name: RegisterName::A,
                                    ..
                                }) => {
                                    self.next()?;
                                    self.here += 1;
                                    self.data.push(0x4A);
                                }

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                                self.next()?;
                                                self.expect_register(RegisterName::X)?;
                                                self.data.push(0x56);
                                            } else {
                                                self.data.push(0x46);
                                            }
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                        self.next()?;
                                        self.expect_register(RegisterName::X)?;
                                        self.data.push(0x5E);
                                    } else {
                                        self.data.push(0x4E);
                                    }
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Nop => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0xEA);
                        }

                        OperationName::Ora => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Symbol {
                                    name: SymbolName::Hash,
                                    ..
                                }) => {
                                    self.next()?;
                                    self.data.push(0x09);
                                    self.expect_immediate()?;
                                    self.here += 2;
                                }

                                Some(Token::Symbol {
                                    name: SymbolName::ParenOpen,
                                    ..
                                }) => {
                                    self.next()?;
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) > (u8::MAX as u32) {
                                            return asm_err!(loc, "Expression result ({value}) will not fit in a byte");
                                        }
                                        value as u8
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::byte(loc, self.data.len() + 1, expr));
                                        0
                                    };
                                    match self.next()? {
                                        None => return self.end_of_input_err(),

                                        Some(Token::Symbol {
                                            name: SymbolName::Comma,
                                            ..
                                        }) => {
                                            self.data.push(0x01);
                                            self.expect_register(RegisterName::X)?;
                                            self.expect_symbol(SymbolName::ParenClose)?;
                                        }

                                        Some(Token::Symbol {
                                            name: SymbolName::ParenClose,
                                            ..
                                        }) => {
                                            self.data.push(0x11);
                                            self.expect_symbol(SymbolName::Comma)?;
                                            self.expect_register(RegisterName::Y)?;
                                        }

                                        Some(tok) => {
                                            return asm_err!(
                                                tok.loc(),
                                                "Unexpected {}, expected \",\" or \")\"",
                                                tok.as_display(&self.str_interner)
                                            );
                                        }
                                    }
                                    self.here += 2;
                                    self.data.push(value);
                                }

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                                self.next()?;
                                                self.expect_register(RegisterName::X)?;
                                                self.data.push(0x15);
                                            } else {
                                                self.data.push(0x05);
                                            }
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                        self.next()?;
                                        match self.next()? {
                                            None => return self.end_of_input_err(),

                                            Some(Token::Register {
                                                name: RegisterName::X,
                                                ..
                                            }) => {
                                                self.data.push(0x1D);
                                            }

                                            Some(Token::Register {
                                                name: RegisterName::Y,
                                                ..
                                            }) => {
                                                self.data.push(0x19);
                                            }

                                            Some(tok) => {
                                                return asm_err!(
                                                    tok.loc(),
                                                    "Unexpected {}, expected register \"x\" or \"y\"",
                                                    tok.as_display(&self.str_interner)
                                                );
                                            }
                                        }
                                    } else {
                                        self.data.push(0x0D);
                                    }
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Pha => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0x48);
                        }

                        OperationName::Php => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0x08);
                        }

                        OperationName::Pla => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0x68);
                        }

                        OperationName::Plp => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0x28);
                        }

                        OperationName::Rol => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Register {
                                    name: RegisterName::A,
                                    ..
                                }) => {
                                    self.next()?;
                                    self.here += 1;
                                    self.data.push(0x2A);
                                }

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                                self.next()?;
                                                self.expect_register(RegisterName::X)?;
                                                self.data.push(0x36);
                                            } else {
                                                self.data.push(0x26);
                                            }
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                        self.next()?;
                                        self.expect_register(RegisterName::X)?;
                                        self.data.push(0x3E);
                                    } else {
                                        self.data.push(0x2E);
                                    }
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Ror => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Register {
                                    name: RegisterName::A,
                                    ..
                                }) => {
                                    self.next()?;
                                    self.here += 1;
                                    self.data.push(0x6A);
                                }

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                                self.next()?;
                                                self.expect_register(RegisterName::X)?;
                                                self.data.push(0x76);
                                            } else {
                                                self.data.push(0x66);
                                            }
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                        self.next()?;
                                        self.expect_register(RegisterName::X)?;
                                        self.data.push(0x7E);
                                    } else {
                                        self.data.push(0x6E);
                                    }
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Rti => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0x40);
                        }

                        OperationName::Rts => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0x60);
                        }

                        OperationName::Sbc => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Symbol {
                                    name: SymbolName::Hash,
                                    ..
                                }) => {
                                    self.next()?;
                                    self.data.push(0xE9);
                                    self.expect_immediate()?;
                                    self.here += 2;
                                }

                                Some(Token::Symbol {
                                    name: SymbolName::ParenOpen,
                                    ..
                                }) => {
                                    self.next()?;
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) > (u8::MAX as u32) {
                                            return asm_err!(loc, "Expression result ({value}) will not fit in a byte");
                                        }
                                        value as u8
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::byte(loc, self.data.len() + 1, expr));
                                        0
                                    };
                                    match self.next()? {
                                        None => return self.end_of_input_err(),

                                        Some(Token::Symbol {
                                            name: SymbolName::Comma,
                                            ..
                                        }) => {
                                            self.data.push(0xE1);
                                            self.expect_register(RegisterName::X)?;
                                            self.expect_symbol(SymbolName::ParenClose)?;
                                        }

                                        Some(Token::Symbol {
                                            name: SymbolName::ParenClose,
                                            ..
                                        }) => {
                                            self.data.push(0xF1);
                                            self.expect_symbol(SymbolName::Comma)?;
                                            self.expect_register(RegisterName::Y)?;
                                        }

                                        Some(tok) => {
                                            return asm_err!(
                                                tok.loc(),
                                                "Unexpected {}, expected \",\" or \")\"",
                                                tok.as_display(&self.str_interner)
                                            );
                                        }
                                    }
                                    self.here += 2;
                                    self.data.push(value);
                                }

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                                self.next()?;
                                                self.expect_register(RegisterName::X)?;
                                                self.data.push(0xF5);
                                            } else {
                                                self.data.push(0xE5);
                                            }
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                        self.next()?;
                                        match self.next()? {
                                            None => return self.end_of_input_err(),

                                            Some(Token::Register {
                                                name: RegisterName::X,
                                                ..
                                            }) => {
                                                self.data.push(0xFD);
                                            }

                                            Some(Token::Register {
                                                name: RegisterName::Y,
                                                ..
                                            }) => {
                                                self.data.push(0xF9);
                                            }

                                            Some(tok) => {
                                                return asm_err!(
                                                    tok.loc(),
                                                    "Unexpected {}, expected register \"x\" or \"y\"",
                                                    tok.as_display(&self.str_interner)
                                                );
                                            }
                                        }
                                    } else {
                                        self.data.push(0xED);
                                    }
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Sec => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0x38);
                        }

                        OperationName::Sed => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0xF8);
                        }

                        OperationName::Sei => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0x78);
                        }

                        OperationName::Sta => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(Token::Symbol {
                                    name: SymbolName::ParenOpen,
                                    ..
                                }) => {
                                    self.next()?;
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) > (u8::MAX as u32) {
                                            return asm_err!(loc, "Expression result ({value}) will not fit in a byte");
                                        }
                                        value as u8
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::byte(loc, self.data.len() + 1, expr));
                                        0
                                    };
                                    match self.next()? {
                                        None => return self.end_of_input_err(),

                                        Some(Token::Symbol {
                                            name: SymbolName::Comma,
                                            ..
                                        }) => {
                                            self.data.push(0x81);
                                            self.expect_register(RegisterName::X)?;
                                            self.expect_symbol(SymbolName::ParenClose)?;
                                        }

                                        Some(Token::Symbol {
                                            name: SymbolName::ParenClose,
                                            ..
                                        }) => {
                                            self.data.push(0x91);
                                            self.expect_symbol(SymbolName::Comma)?;
                                            self.expect_register(RegisterName::Y)?;
                                        }

                                        Some(tok) => {
                                            return asm_err!(
                                                tok.loc(),
                                                "Unexpected {}, expected \",\" or \")\"",
                                                tok.as_display(&self.str_interner)
                                            );
                                        }
                                    }
                                    self.here += 2;
                                    self.data.push(value);
                                }

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                                self.next()?;
                                                self.expect_register(RegisterName::X)?;
                                                self.data.push(0x95);
                                            } else {
                                                self.data.push(0x85);
                                            }
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                        self.next()?;
                                        match self.next()? {
                                            None => return self.end_of_input_err(),

                                            Some(Token::Register {
                                                name: RegisterName::X,
                                                ..
                                            }) => {
                                                self.data.push(0x9D);
                                            }

                                            Some(Token::Register {
                                                name: RegisterName::Y,
                                                ..
                                            }) => {
                                                self.data.push(0x99);
                                            }

                                            Some(tok) => {
                                                return asm_err!(
                                                    tok.loc(),
                                                    "Unexpected {}, expected register \"x\" or \"y\"",
                                                    tok.as_display(&self.str_interner)
                                                );
                                            }
                                        }
                                    } else {
                                        self.data.push(0x8D);
                                    }
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Stx => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                                self.next()?;
                                                self.expect_register(RegisterName::Y)?;
                                                self.data.push(0x96);
                                            } else {
                                                self.data.push(0x86);
                                            }
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    self.data.push(0x8E);
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Sty => {
                            self.next()?;
                            match self.peek()? {
                                None => return self.end_of_input_err(),

                                Some(_) => {
                                    let (loc, expr) = self.expr()?;
                                    let value = if let Some(value) = expr.evaluate(&self.symtab) {
                                        if (value as u32) <= (u8::MAX as u32) {
                                            self.here += 2;
                                            if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                                self.next()?;
                                                self.expect_register(RegisterName::X)?;
                                                self.data.push(0x94);
                                            } else {
                                                self.data.push(0x84);
                                            }
                                            self.data.push(value as u8);
                                            continue;
                                        }
                                        if (value as u32) > (u16::MAX as u32) {
                                            return asm_err!(
                                                loc,
                                                "Expression result ({value}) will not fit in a word"
                                            );
                                        }
                                        value
                                    } else {
                                        // We need to add 1 since we havent written the opcode yet :|
                                        self.links.push(Link::word(loc, self.data.len() + 1, expr));
                                        0
                                    };

                                    self.here += 3;
                                    self.data.push(0x8C);
                                    self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                }
                            }
                        }

                        OperationName::Tax => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0xAA);
                        }

                        OperationName::Tay => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0xA8);
                        }

                        OperationName::Tsx => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0xBA);
                        }

                        OperationName::Txa => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0x8A);
                        }

                        OperationName::Txs => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0x9A);
                        }

                        OperationName::Tya => {
                            self.next()?;
                            self.here += 1;
                            self.data.push(0x98);
                        }
                    }
                }

                Some(&tok) => {
                    return asm_err!(
                        tok.loc(),
                        "Unexpected {}",
                        tok.as_display(&self.str_interner)
                    )
                }
            }
        }
    }
}

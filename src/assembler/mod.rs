use std::{
    borrow::Borrow,
    cell::RefCell,
    fmt::Write,
    io::{Cursor, Read},
    iter,
    marker::PhantomData,
    path::Path,
    rc::Rc,
};

use fxhash::FxHashMap;

use crate::{
    expr::{Expr, ExprNode},
    fileman::{FileManager, FileSystem},
    intern::{PathRef, StrInterner, StrRef},
    lexer::{
        ArchTokens, DirectiveName, LabelKind, Lexer, LexerError, SourceLoc, SymbolName, Token,
    },
    linker::{Link, Module},
    symtab::{Symbol, Symtab},
};

#[cfg(test)]
mod tests;

macro_rules! asm_err {
    ($loc:expr, $($arg:tt)*) => {
        Err(($loc, AssemblerError(format!($($arg)*))))
    };
}

enum TokenSource<R, A: ArchTokens> {
    Lexer(Lexer<R, A>),
    ParseLexer(Lexer<Cursor<String>, A>),
    Macro(MacroState<A>),
}

impl<R: Read, A: ArchTokens> TokenSource<R, A> {
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
        macros: &mut FxHashMap<StrRef, Macro<A>>,
    ) -> Option<Result<Token<A>, (SourceLoc, AssemblerError)>> {
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

                        MacroToken::Argument { index, .. } => {
                            state.expanding_macro_arg = Some(index);
                            state.macro_arg_offset = 0;
                            continue;
                        }
                    }
                }
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum MacroToken<A: ArchTokens> {
    Token(Token<A>),
    Argument { loc: SourceLoc, index: usize },
}

struct Macro<A: ArchTokens> {
    loc: SourceLoc,
    args: Vec<StrRef>,
    tokens: Vec<MacroToken<A>>,
}

#[derive(thiserror::Error, Debug)]
#[error("{0}")]
pub struct AssemblerError(pub String);

impl From<LexerError> for (SourceLoc, AssemblerError) {
    fn from(e: LexerError) -> Self {
        (e.loc(), AssemblerError(format!("{e}")))
    }
}

struct MacroState<A: ArchTokens> {
    name: StrRef,
    args: Vec<Vec<Token<A>>>,
    macro_offset: usize,
    expanding_macro_arg: Option<usize>,
    macro_arg_offset: usize,
    loc: SourceLoc,
    included_from: Option<SourceLoc>,
    str_interner: Rc<RefCell<StrInterner>>,
}

enum SegmentMode {
    Code,
    Addr,
}

pub struct Assembler<S, R, A: ArchTokens, Z> {
    file_manager: FileManager<S>,
    pub(crate) str_interner: Rc<RefCell<StrInterner>>,
    token_sources: Vec<TokenSource<R, A>>,
    token_source: Option<TokenSource<R, A>>,
    cwds: Vec<PathRef>,
    cwd: Option<PathRef>,
    macros: FxHashMap<StrRef, Macro<A>>,
    pub(crate) symtab: Symtab,
    pub(crate) data: Vec<u8>,
    seg_mode: SegmentMode,
    pub(crate) links: Vec<Link>,

    entropy: usize,
    stash: Option<Token<A>>,
    loc: Option<SourceLoc>,
    pub(crate) here: u32,
    active_namespace: Option<StrRef>,
    active_macro: Option<StrRef>,
    if_level: usize,

    marker: PhantomData<Z>,
}

pub trait ArchAssembler<S, R, A: ArchTokens> {
    fn parse(
        asm: &mut Assembler<S, R, A, Self>,
        name: A::OperationName,
    ) -> Result<(), (SourceLoc, AssemblerError)>
    where
        Self: Sized;
}

impl<S, R, A, Z> Assembler<S, R, A, Z>
where
    S: FileSystem<Reader = R>,
    R: Read,
    A: ArchTokens,
    Z: ArchAssembler<S, R, A>,
{
    pub fn new(file_system: S, _: Z) -> Self {
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
            seg_mode: SegmentMode::Code,
            links: Vec::new(),

            entropy: 0,
            stash: None,
            loc: None,
            here: 0,
            active_namespace: None,
            active_macro: None,
            if_level: 0,

            marker: PhantomData,
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

    pub(crate) fn peek(&mut self) -> Result<Option<Token<A>>, (SourceLoc, AssemblerError)> {
        loop {
            if self.token_source.is_none() {
                self.token_source = self.token_sources.pop();
                self.cwd = self.cwds.pop();
            }
            match self.stash {
                Some(_) => return Ok(self.stash),

                None => match &mut self.token_source {
                    None => return Ok(None),

                    Some(token_source) => {
                        let tok = token_source.next(&mut self.macros).transpose()?;
                        // Note: we intentionally do not skip newlines and comments.
                        // we want to pass them on to expression parsing! They act as an
                        // "epsilon" token to terminate a non-terminal expression!
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
                                    self.cwds.push(self.cwd.unwrap());

                                    self.token_sources.push(TokenSource::Macro(mac));
                                    self.cwds.push(self.cwd.take().unwrap());
                                    continue;
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
                                    self.cwds.push(self.cwd.unwrap());

                                    self.token_sources.push(TokenSource::Macro(mac));
                                    self.cwds.push(self.cwd.take().unwrap());
                                    continue;
                                }

                                Some(Token::Directive {
                                    loc,
                                    name: DirectiveName::GetMeta,
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
                                                        let global =
                                                            interner.get(namespace).unwrap();
                                                        let label = interner.get(value).unwrap();
                                                        format!("{global}{label}")
                                                    };
                                                    self.str_interner
                                                        .borrow_mut()
                                                        .intern(global_label)
                                                } else {
                                                    let interner =
                                                        self.str_interner.as_ref().borrow();
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
                                        if let Some(meta) =
                                            self.symtab.meta_interner().get(sym.meta())
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
                                        .intern(format!("@metaget Invocation{}", self.entropy));
                                    self.entropy += 1;
                                    self.macros.insert(
                                        name,
                                        Macro {
                                            loc,
                                            args: Vec::new(),
                                            tokens: toks,
                                        },
                                    );

                                    self.token_sources.push(self.token_source.take().unwrap());
                                    self.cwds.push(self.cwd.unwrap());

                                    self.token_sources.push(TokenSource::Macro(MacroState {
                                        name,
                                        args: Vec::new(),
                                        macro_offset: 0,
                                        expanding_macro_arg: None,
                                        macro_arg_offset: 0,
                                        loc,
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
                                    self.cwds.push(self.cwd.unwrap());

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
                                    self.cwds.push(self.cwd.unwrap());

                                    for _ in 0..states.len() {
                                        self.cwds.push(self.cwd.unwrap());
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
                                    let mac = self.expect_number_directive_arg(
                                        loc,
                                        DirectiveName::Hex,
                                        16,
                                    )?;

                                    self.token_sources.push(self.token_source.take().unwrap());
                                    self.cwds.push(self.cwd.unwrap());

                                    self.token_sources.push(TokenSource::Macro(mac));
                                    self.cwds.push(self.cwd.take().unwrap());
                                    continue;
                                }

                                Some(Token::Directive {
                                    loc,
                                    name: DirectiveName::Bin,
                                }) => {
                                    let mac = self.expect_number_directive_arg(
                                        loc,
                                        DirectiveName::Bin,
                                        2,
                                    )?;

                                    self.token_sources.push(self.token_source.take().unwrap());
                                    self.cwds.push(self.cwd.unwrap());

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
                                                        let global =
                                                            interner.get(namespace).unwrap();
                                                        let label = interner.get(value).unwrap();
                                                        format!("{global}{label}")
                                                    };
                                                    self.str_interner
                                                        .borrow_mut()
                                                        .intern(global_label)
                                                } else {
                                                    let interner =
                                                        self.str_interner.as_ref().borrow();
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

    pub(crate) fn next(&mut self) -> Result<Option<Token<A>>, (SourceLoc, AssemblerError)> {
        self.peek()?;
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

    #[inline]
    pub(crate) fn end_of_input_err<T>(&mut self) -> Result<T, (SourceLoc, AssemblerError)> {
        asm_err!(self.loc(), "Unexpected end of input")
    }

    #[inline]
    pub(crate) fn expect_symbol(
        &mut self,
        sym: SymbolName,
    ) -> Result<(), (SourceLoc, AssemblerError)> {
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
    pub(crate) fn expect_register(
        &mut self,
        reg: A::RegisterName,
    ) -> Result<(), (SourceLoc, AssemblerError)> {
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

    pub(crate) fn expect_immediate(&mut self) -> Result<(), (SourceLoc, AssemblerError)> {
        let (loc, expr) = self.expr()?;
        if let Some(value) = expr.evaluate(&self.symtab, &self.str_interner) {
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

    pub(crate) fn expect_hmem_immediate(&mut self) -> Result<(), (SourceLoc, AssemblerError)> {
        let (loc, expr) = self.expr()?;
        if let Some(value) = expr.evaluate(&self.symtab, &self.str_interner) {
            if (value as u32) > (u8::MAX as u32) {
                if (value as u32) > (u16::MAX as u32) {
                    return asm_err!(loc, "Expression result ({value}) will not fit in a word");
                }
                if !(0xFF00..=0xFFFF).contains(&value) {
                    return asm_err!(
                        loc,
                        "Expression result ({value}) must be between $FF00 and $FFFF"
                    );
                }
            }
            self.data.push(value as u8);
        } else {
            self.links.push(Link::byte(loc, self.data.len(), expr));
            self.data.push(0);
        }
        Ok(())
    }

    pub(crate) fn expect_wide_immediate(&mut self) -> Result<(), (SourceLoc, AssemblerError)> {
        let (loc, expr) = self.expr()?;
        if let Some(value) = expr.evaluate(&self.symtab, &self.str_interner) {
            if (value as u32) > (u16::MAX as u32) {
                return asm_err!(loc, "Expression result ({value}) will not fit in a word");
            }
            self.data.extend_from_slice(&(value as u16).to_le_bytes());
        } else {
            self.links.push(Link::word(loc, self.data.len(), expr));
            self.data.push(0);
            self.data.push(0);
        }
        Ok(())
    }

    pub(crate) fn expect_branch_immediate(&mut self) -> Result<(), (SourceLoc, AssemblerError)> {
        let (loc, mut expr) = self.expr()?;
        expr.push(ExprNode::Value(self.here.wrapping_add(2) as i32)); // subtract where the PC will be
        expr.push(ExprNode::Sub);
        if let Some(value) = expr.evaluate(&self.symtab, &self.str_interner) {
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
    ) -> Result<MacroState<A>, (SourceLoc, AssemblerError)> {
        let mut args = Vec::new();
        let arg_count = self.macros.get(&name).unwrap().args.len();
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

        Ok(MacroState {
            name,
            args,
            macro_offset: 0,
            expanding_macro_arg: None,
            macro_arg_offset: 0,
            loc,
            included_from: Some(loc),
            str_interner: self.str_interner.clone(),
        })
    }

    fn expect_string_directive_arg(
        &mut self,
        loc: SourceLoc,
    ) -> Result<Token<A>, (SourceLoc, AssemblerError)> {
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
    ) -> Result<MacroState<A>, (SourceLoc, AssemblerError)> {
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
        let name = interner.intern(format!("{directive_name} Invocation{}", self.entropy));
        self.entropy += 1;
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
                args: Vec::new(),
                tokens: toks,
            },
        );

        Ok(MacroState {
            name,
            args: Vec::new(),
            macro_offset: 0,
            expanding_macro_arg: None,
            macro_arg_offset: 0,
            loc,
            included_from: Some(loc),
            str_interner: self.str_interner.clone(),
        })
    }

    fn expect_count_directive(
        &mut self,
        loc: SourceLoc,
    ) -> Result<MacroState<A>, (SourceLoc, AssemblerError)> {
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
            .intern(format!("@count Invocation{}", self.entropy));
        self.entropy += 1;
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
                args: Vec::new(),
                tokens: toks,
            },
        );

        Ok(MacroState {
            name,
            args: Vec::new(),
            macro_offset: 0,
            expanding_macro_arg: None,
            macro_arg_offset: 0,
            loc,
            included_from: Some(loc),
            str_interner: self.str_interner.clone(),
        })
    }

    fn expect_label_directive_arg(
        &mut self,
        loc: SourceLoc,
    ) -> Result<Token<A>, (SourceLoc, AssemblerError)> {
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
    ) -> Result<Vec<MacroState<A>>, (SourceLoc, AssemblerError)> {
        let arg_value = match self.next()? {
            None => return self.end_of_input_err(),

            Some(Token::Label { loc, kind, value }) => {
                let interner = self.str_interner.as_ref().borrow();
                let str_value = interner.get(value).unwrap();
                if kind != LabelKind::Global {
                    return asm_err!(
                        loc,
                        "\"@each\" label \"{str_value}\" must be a global label",
                    );
                }
                value
            }

            Some(tok) => {
                return asm_err!(
                    tok.loc(),
                    "Unexpected {}, expected an \"@each\" token placeholder name",
                    tok.as_display(&self.str_interner)
                );
            }
        };

        self.expect_symbol(SymbolName::Comma)?;

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
            .intern(format!("@each Invocation{}", self.entropy));
        self.entropy += 1;
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

                Some(Token::Label { loc, kind, value })
                    if kind == LabelKind::Global && value == arg_value =>
                {
                    toks.push(MacroToken::Argument { loc, index: 0 });
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
                args: vec![arg_value], // the current token
                tokens: toks,
            },
        );

        let mut states = Vec::new();
        for arg in args {
            states.push(MacroState {
                name,
                args: vec![vec![arg]],
                macro_offset: 0,
                expanding_macro_arg: None,
                macro_arg_offset: 0,
                loc,
                included_from: Some(loc),
                str_interner: self.str_interner.clone(),
            });
        }
        Ok(states)
    }

    pub(crate) fn const_expr(
        &mut self,
    ) -> Result<(SourceLoc, Option<i32>), (SourceLoc, AssemblerError)> {
        self.expr()
            .map(|(loc, expr)| (loc, expr.evaluate(&self.symtab, &self.str_interner)))
    }

    pub(crate) fn expr(&mut self) -> Result<(SourceLoc, Expr), (SourceLoc, AssemblerError)> {
        let mut nodes = Vec::new();
        let loc = self.expr_prec_0(&mut nodes)?;
        Ok((loc, Expr::new(nodes)))
    }

    fn expr_prec_0(
        &mut self,
        nodes: &mut Vec<ExprNode>,
    ) -> Result<SourceLoc, (SourceLoc, AssemblerError)> {
        let loc = self.expr_prec_1(nodes)?;

        loop {
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
                    return Ok(loc);
                }

                _ => return Ok(loc),
            }
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
        loop {
            match self.peek()? {
                None => return self.end_of_input_err(),
                Some(Token::Symbol {
                    loc,
                    name: SymbolName::Minus,
                }) => {
                    self.next()?;
                    self.expr_prec_11(nodes)?;
                    nodes.push(ExprNode::Neg);
                    return Ok(loc);
                }
                Some(Token::Symbol {
                    loc,
                    name: SymbolName::Bang,
                }) => {
                    self.next()?;
                    self.expr_prec_11(nodes)?;
                    nodes.push(ExprNode::NotLogical);
                    return Ok(loc);
                }
                Some(Token::Symbol {
                    loc,
                    name: SymbolName::Tilde,
                }) => {
                    self.next()?;
                    self.expr_prec_11(nodes)?;
                    nodes.push(ExprNode::Invert);
                    return Ok(loc);
                }
                Some(Token::Symbol {
                    loc,
                    name: SymbolName::LessThan,
                }) => {
                    self.next()?;
                    self.expr_prec_11(nodes)?;
                    nodes.push(ExprNode::Lo);
                    return Ok(loc);
                }
                Some(Token::Symbol {
                    loc,
                    name: SymbolName::GreaterThan,
                }) => {
                    self.next()?;
                    self.expr_prec_11(nodes)?;
                    nodes.push(ExprNode::Hi);
                    return Ok(loc);
                }
                Some(Token::Symbol {
                    loc,
                    name: SymbolName::ParenOpen,
                }) => {
                    self.next()?;
                    self.expr_prec_0(nodes)?;
                    if self.peeked_symbol(SymbolName::ParenClose)?.is_none() {
                        return asm_err!(self.loc(), "Expected a \")\" to close expression");
                    }
                    self.next()?;
                    return Ok(loc);
                }
                Some(Token::Number { loc, value }) => {
                    self.next()?;
                    nodes.push(ExprNode::Value(value as i32));
                    return Ok(loc);
                }
                Some(Token::Directive { loc, name }) => match name {
                    DirectiveName::Here => {
                        self.next()?;
                        nodes.push(ExprNode::Value(self.here as i32));
                        return Ok(loc);
                    }

                    DirectiveName::SizeOf => {
                        self.next()?;
                        match self.next()? {
                            None => return self.end_of_input_err(),

                            Some(Token::Label { loc, kind, value }) => {
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
                                            return asm_err!(loc, "The local label \"{label}\" is being evaluated but there was no global label defined before it");
                                        }
                                    }
                                };

                                nodes.push(ExprNode::SizeOf(direct));
                                // Important to record where in expressions we reference
                                // symbols, so we can barf at link time
                                self.symtab.touch(direct, loc);
                                return Ok(loc);
                            }

                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected a struct field label",
                                    tok.as_display(&self.str_interner)
                                )
                            }
                        }
                    }

                    _ => {
                        return asm_err!(
                            loc,
                            "\"{name}\" directives are not allowed in expressions"
                        )
                    }
                },
                Some(Token::Label { loc, kind, value }) => {
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
                                return asm_err!(loc, "The local label \"{label}\" is being evaluated but there was no global label defined before it");
                            }
                        }
                    };

                    if let Some(sym) = self.symtab.get(direct) {
                        match sym.inner() {
                            Symbol::Value(value) => {
                                nodes.push(ExprNode::Value(*value));
                            }
                            Symbol::Expr(expr) => {
                                if let Some(value) = expr.evaluate(&self.symtab, &self.str_interner)
                                {
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
                    return Ok(loc);
                }
                Some(tok) => {
                    return asm_err!(
                        tok.loc(),
                        "Unexpected {} in expression",
                        tok.as_display(&self.str_interner)
                    )
                }
            }
        }
    }

    #[inline]
    pub(crate) fn peeked_symbol(
        &mut self,
        sym: SymbolName,
    ) -> Result<Option<Token<A>>, (SourceLoc, AssemblerError)> {
        match self.peek()? {
            Some(tok @ Token::Symbol { name, .. }) if name == sym => Ok(Some(tok)),
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

                Some(Token::Label { loc, value, kind }) => {
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

                Some(tok @ Token::Directive { loc, name }) => {
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
                                Some(Token::String { value, ..  }) => {
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

                                Some(Token::String { value, .. }) => {
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

                            if let Some(value) = expr.evaluate(&self.symtab, &self.str_interner) {
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

                                Some(Token::Label { loc, value, kind }) => match kind {
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

                                Some(Token::Label { loc, value, kind }) => match kind {
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
                                return asm_err!(
                                    loc,
                                    "The constant \"{label}\" was already defined"
                                );
                            }

                            self.expect_symbol(SymbolName::Comma)?;
                            let (_, expr) = self.expr()?;
                            self.symtab
                                .insert_with_meta(direct, Symbol::Expr(expr), &[]);
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
                            self.symtab
                                .insert_with_meta(direct, Symbol::Expr(expr), &[]);
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

                            match self.seg_mode {
                                SegmentMode::Addr => {
                                    self.here += 1;
                                }

                                SegmentMode::Code => loop {
                                    match self.peek()? {
                                        Some(Token::String { loc, value, .. }) => {
                                            self.next()?;
                                            let interner = self.str_interner.as_ref().borrow();
                                            let bytes = interner.get(value).unwrap().as_bytes();

                                            if (self.here as usize) + bytes.len()
                                                > (u16::MAX as usize)
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
                                            if let Some(value) =
                                                expr.evaluate(&self.symtab, &self.str_interner)
                                            {
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
                                                self.links.push(Link::byte(
                                                    loc,
                                                    self.data.len(),
                                                    expr,
                                                ));
                                                self.data.push(0);
                                            }
                                        }
                                    }

                                    if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                        self.next()?;
                                        continue;
                                    }
                                    break;
                                },
                            }
                        }

                        DirectiveName::Dw => {
                            self.next()?;

                            match self.seg_mode {
                                SegmentMode::Addr => {
                                    self.here += 2;
                                }

                                SegmentMode::Code => loop {
                                    self.peek()?;
                                    let (loc, expr) = self.expr()?;
                                    if let Some(value) =
                                        expr.evaluate(&self.symtab, &self.str_interner)
                                    {
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
                                        self.data.extend_from_slice(&(value as u16).to_le_bytes());
                                    } else {
                                        self.here += 2;
                                        self.links.push(Link::word(loc, self.data.len(), expr));
                                        self.data.push(0);
                                        self.data.push(0);
                                    }

                                    if self.peeked_symbol(SymbolName::Comma)?.is_some() {
                                        self.next()?;
                                        continue;
                                    }
                                    break;
                                },
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

                            match self.seg_mode {
                                SegmentMode::Addr => {}

                                SegmentMode::Code => {
                                    let value = if self.peeked_symbol(SymbolName::Comma)?.is_some()
                                    {
                                        self.next()?;
                                        let (loc, expr) = self.expr()?;
                                        if let Some(value) =
                                            expr.evaluate(&self.symtab, &self.str_interner)
                                        {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(
                                            loc,
                                            "\"@ds\" value expression result ({value}) will not fit in a byte"
                                        );
                                            }
                                            value as u8
                                        } else {
                                            self.links.push(Link::space(
                                                loc,
                                                self.data.len(),
                                                size,
                                                expr,
                                            ));
                                            0
                                        }
                                    } else {
                                        0
                                    };
                                    self.data.extend(iter::repeat(value).take(size));
                                }
                            }
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

                        DirectiveName::Segment => {
                            self.next()?;
                            match self.next()? {
                                None => return self.end_of_input_err(),

                                Some(Token::String { loc, value }) => {
                                    let interner = self.str_interner.borrow_mut();
                                    if interner.eq_some("CODE", value)
                                        || interner.eq_some("code", value)
                                    {
                                        self.seg_mode = SegmentMode::Code
                                    } else if interner.eq_some("ADDR", value)
                                        || interner.eq_some("addr", value)
                                    {
                                        self.seg_mode = SegmentMode::Addr
                                    } else {
                                        let value = interner.get(value).unwrap();
                                        return asm_err!(
                                            loc,
                                            "Unrecognized segment name \"{value}\", only \"CODE\" or \"ADDR\" are valid segment names",
                                        );
                                    }
                                }

                                Some(tok) => {
                                    return asm_err!(
                                        tok.loc(),
                                        "Unexpected {}, expected segment name string",
                                        tok.as_display(&self.str_interner)
                                    );
                                }
                            }
                        }

                        DirectiveName::Incbin => {
                            if let SegmentMode::Addr = self.seg_mode {
                                return asm_err!(
                                    self.loc(),
                                    "Binary data cannot be included in an \"ADDR\" segment"
                                );
                            }
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

                            let args_count = match self.next()? {
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

                            let mut args = Vec::new();
                            for _ in 0..args_count {
                                self.expect_symbol(SymbolName::Comma)?;

                                match self.next()? {
                                    None => return self.end_of_input_err(),

                                    Some(Token::Label { loc, kind, value }) => {
                                        let interner = self.str_interner.as_ref().borrow();
                                        let str_value = interner.get(value).unwrap();
                                        if kind != LabelKind::Global {
                                            return asm_err!(
                                                loc,
                                                "\"@macro\" argument name \"{str_value}\" must be a global label"
                                            );
                                        }
                                        args.push(value);
                                    }

                                    Some(tok) => {
                                        return asm_err!(
                                            tok.loc(),
                                            "Unexpected {}, expected a macro argument name",
                                            tok.as_display(&self.str_interner)
                                        );
                                    }
                                };
                            }

                            self.active_macro = Some(value);
                            let mut toks = Vec::new();
                            let mut macro_depth = 0;
                            loop {
                                match self.next()? {
                                    None => return self.end_of_input_err(),
                                    Some(Token::Comment { .. } | Token::NewLine { .. }) => {}

                                    Some(
                                        tok @ Token::Directive {
                                            name: DirectiveName::Macro,
                                            ..
                                        },
                                    ) => {
                                        macro_depth += 1;
                                        toks.push(MacroToken::Token(tok));
                                    }

                                    Some(
                                        tok @ Token::Directive {
                                            name: DirectiveName::EndMacro,
                                            ..
                                        },
                                    ) => {
                                        if macro_depth == 0 {
                                            self.active_macro = None;
                                            break;
                                        }
                                        macro_depth -= 1;
                                        toks.push(MacroToken::Token(tok));
                                    }

                                    Some(Token::Label { loc, kind, value })
                                        if kind == LabelKind::Global && args.contains(&value) =>
                                    {
                                        toks.push(MacroToken::Argument {
                                            loc,
                                            index: args
                                                .iter()
                                                .position(|arg| arg == &value)
                                                .unwrap(),
                                        })
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
                                                    let mut interner =
                                                        self.str_interner.borrow_mut();
                                                    let key = interner.intern("@SIZEOF");
                                                    let value =
                                                        interner.intern(format!("{field_size}"));
                                                    self.symtab.insert_with_meta(
                                                        direct,
                                                        Symbol::Value(struct_size),
                                                        &[[key, value]],
                                                    );
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
                            self.symtab
                                .insert_with_meta(value, Symbol::Value(struct_size), &[]);
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
                                                tok.as_display(&self.str_interner)
                                            );
                                        }
                                    },

                                    Some(tok) => {
                                        return asm_err!(
                                            tok.loc(),
                                            "Unexpected {}, expected a metadata key",
                                            tok.as_display(&self.str_interner)
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

                        DirectiveName::If => {
                            self.next()?;

                            let result = match self.const_expr()? {
                                (_, Some(value)) => value,
                                (loc, None) => {
                                    return asm_err!(
                                        loc,
                                        "The expression following an \"@if\" directive must be immediately solvable"
                                    )
                                }
                            };

                            if result != 0 {
                                self.if_level += 1;
                            } else {
                                let mut if_level = 1;
                                loop {
                                    match self.next()? {
                                        None => return self.end_of_input_err(),

                                        Some(Token::Directive {
                                            name: DirectiveName::If,
                                            ..
                                        }) => {
                                            if_level += 1;
                                        }

                                        Some(Token::Directive {
                                            name: DirectiveName::EndIf,
                                            ..
                                        }) => {
                                            if_level -= 1;
                                            if if_level == 0 {
                                                break;
                                            }
                                        }

                                        Some(_) => {}
                                    }
                                }
                            }
                        }

                        DirectiveName::EndIf => {
                            self.next()?;
                            if self.if_level == 0 {
                                return asm_err!(tok.loc(), "Unexpected \"@endif\"");
                            }
                            self.if_level -= 1;
                        }

                        _ => {
                            return asm_err!(
                                tok.loc(),
                                "Unexpected {}",
                                tok.as_display(&self.str_interner)
                            );
                        }
                    }
                }

                Some(Token::Operation { name, .. }) => {
                    if let SegmentMode::Addr = self.seg_mode {
                        return asm_err!(
                            self.loc(),
                            "Cannot place instructions in an \"ADDR\" segment",
                        );
                    }

                    let old_len = self.data.len();
                    <Z as ArchAssembler<S, R, A>>::parse(self, name)?;
                    self.here += (self.data.len() - old_len) as u32;
                }

                Some(tok) => {
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

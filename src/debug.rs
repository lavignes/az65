use std::{cell::RefCell, io::Write, marker::PhantomData, path::Path, rc::Rc};

use fxhash::FxHashMap;

use crate::{
    fileman::{FileManager, FileSystem},
    intern::StrInterner,
    symtab::{Symbol, Symtab},
};

#[derive(thiserror::Error, Debug)]
#[error("{0}")]
pub struct DebugExporterError(String);

impl DebugExporterError {
    #[inline]
    pub fn new(msg: String) -> Self {
        Self(msg)
    }
}

pub trait DebugExporter {
    type FileSystem: FileSystem;

    fn export(
        &mut self,
        file_manager: &mut FileManager<Self::FileSystem>,
        str_interner: &Rc<RefCell<StrInterner>>,
        symtab: &Symtab,
        cwd: &Path,
        path: &Path,
    ) -> Result<(), DebugExporterError>;
}

pub struct AZ65Meta<S> {
    marker: PhantomData<S>,
}

impl<S> AZ65Meta<S> {
    #[inline]
    pub fn new() -> Self {
        Self {
            marker: PhantomData,
        }
    }
}

#[derive(microserde::Serialize)]
struct DebugSymbol {
    name: String,
    value: i32,
    meta: FxHashMap<String, String>,
}

impl<S> DebugExporter for AZ65Meta<S>
where
    S: FileSystem,
{
    type FileSystem = S;

    fn export(
        &mut self,
        file_manager: &mut FileManager<Self::FileSystem>,
        str_interner: &Rc<RefCell<StrInterner>>,
        symtab: &Symtab,
        cwd: &Path,
        path: &Path,
    ) -> Result<(), DebugExporterError> {
        let mut symbols = Vec::new();
        for (strref, _) in symtab {
            let sym = symtab.get(*strref).unwrap();
            let value = match sym.inner() {
                Symbol::Value(value) => *value,
                Symbol::Expr(expr) => expr.evaluate(symtab, str_interner).unwrap(),
            };

            let interner = str_interner.as_ref().borrow();
            let mut meta_map = FxHashMap::default();
            let meta = symtab.meta_interner().get(sym.meta()).unwrap();
            for pair in meta {
                let key = interner.get(pair[0]).unwrap();
                let value = interner.get(pair[1]).unwrap();
                meta_map.insert(key.into(), value.into());
            }

            symbols.push(DebugSymbol {
                name: interner.get(*strref).unwrap().into(),
                value,
                meta: meta_map,
            });
        }

        let (_, mut writer) = match file_manager.writer(cwd, path) {
            Ok(tup) => tup,
            Err(e) => {
                return Err(DebugExporterError::new(format!(
                    "Failed to open \"{}\" for writing: {e}",
                    path.display()
                )));
            }
        };

        if let Err(e) = write!(writer, "{}", microserde::json::to_string(&symbols)) {
            return Err(DebugExporterError::new(format!(
                "Failed to write to \"{}\": {e}",
                path.display()
            )));
        }
        Ok(())
    }
}

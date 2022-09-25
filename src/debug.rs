use std::{
    cell::Ref,
    io::{Read, Write},
    marker::PhantomData,
    path::Path,
};

use fxhash::FxHashMap;

use crate::{
    fileman::{FileManager, FileSystem},
    intern::StrInterner,
    linker::{DebugExporter, DebugExporterError},
    symtab::{Symbol, Symtab},
};

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

impl<S, R, W> DebugExporter for AZ65Meta<S>
where
    S: FileSystem<Reader = R, Writer = W>,
    R: Read,
    W: Write,
{
    type FileSystem = S;

    fn export(
        &mut self,
        file_manager: &mut FileManager<Self::FileSystem>,
        str_interner: Ref<StrInterner>,
        symtab: &Symtab,
        cwd: &Path,
        path: &Path,
    ) -> Result<(), DebugExporterError> {
        let mut symbols = Vec::new();
        for (strref, _) in symtab {
            let sym = symtab.get(*strref).unwrap();
            let value = match sym.inner() {
                Symbol::Value(value) => *value,
                Symbol::Expr(expr) => expr.evaluate(symtab).unwrap(),
            };

            let mut meta_map = FxHashMap::default();
            let meta = symtab.meta_interner().get(sym.meta()).unwrap();
            for pair in meta {
                let key = str_interner.get(pair[0]).unwrap();
                let value = str_interner.get(pair[1]).unwrap();
                meta_map.insert(key.into(), value.into());
            }

            symbols.push(DebugSymbol {
                name: str_interner.get(*strref).unwrap().into(),
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

use std::{io::Write, marker::PhantomData, path::Path};

use fxhash::FxHashMap;

use crate::{
    debug::{DebugExporter, DebugExporterError},
    fileman::{FileManager, FileSystem},
    intern::StrInterner,
    symtab::{Symbol, Symtab},
};

pub struct NameList<S> {
    marker: PhantomData<S>,
}

impl<S> NameList<S> {
    #[inline]
    pub fn new() -> Self {
        Self {
            marker: PhantomData,
        }
    }
}

impl<S> DebugExporter for NameList<S>
where
    S: FileSystem,
{
    type FileSystem = S;

    fn export(
        &mut self,
        file_manager: &mut FileManager<Self::FileSystem>,
        str_interner: &StrInterner,
        symtab: &Symtab,
        cwd: &Path,
        path: &Path,
    ) -> Result<(), DebugExporterError> {
        let mut ram_entries = Vec::new();
        let mut prg_banks = FxHashMap::default();

        for (strref, _) in symtab {
            let sym = symtab.get(*strref).unwrap();
            let value = match sym.inner() {
                Symbol::Value(value) => *value,
                Symbol::Expr(expr) => expr.evaluate(symtab).unwrap(),
            };
            let meta = symtab.meta_interner().get(sym.meta()).unwrap();

            let mut ram = false;
            let mut prg = false;
            let mut bank = None;
            for pair in meta {
                let key = str_interner.get(pair[0]).unwrap();
                let value = str_interner.get(pair[1]).unwrap();
                match (key, value) {
                    ("ID", "ZP") => ram = true,
                    ("ID", "RAM") => ram = true,
                    ("ID", "PRG") => prg = true,
                    ("BANK", value) => {
                        if let Ok(value) = usize::from_str_radix(value, 16) {
                            bank = Some(value);
                        }
                    }
                    _ => {}
                }
            }

            if ram {
                ram_entries.push((strref, value as u16))
            }

            if let Some(bank) = bank {
                if prg {
                    prg_banks
                        .entry(bank)
                        .or_insert_with(Vec::new)
                        .push((strref, value as u16));
                }
            }
        }

        if !ram_entries.is_empty() {
            let mut path = path.to_path_buf();
            let mut extension = path.extension().unwrap_or_default().to_os_string();
            extension.push(".ram.nl");
            path.set_extension(extension);
            let (_, mut writer) = match file_manager.writer(&cwd, &path) {
                Ok(tup) => tup,
                Err(e) => {
                    return Err(DebugExporterError::new(format!(
                        "Failed to open \"{}\" for wrting: {e}",
                        path.display()
                    )));
                }
            };

            for (label, value) in ram_entries {
                let label = str_interner.get(*label).unwrap();
                if let Err(e) = writeln!(writer, "${value:04X}#{label}#") {
                    return Err(DebugExporterError::new(format!(
                        "Failed to write to \"{}\": {e}",
                        path.display()
                    )));
                }
            }
        }

        for (bank, entries) in prg_banks {
            let mut path = path.to_path_buf();
            let mut extension = path.extension().unwrap_or_default().to_os_string();
            extension.push(format!(".{bank:X}.nl"));
            path.set_extension(extension);
            let (_, mut writer) = match file_manager.writer(&cwd, &path) {
                Ok(tup) => tup,
                Err(e) => {
                    return Err(DebugExporterError::new(format!(
                        "Failed to open \"{}\" for writing: {e}",
                        path.display()
                    )));
                }
            };

            for (label, value) in entries {
                let label = str_interner.get(*label).unwrap();
                if let Err(e) = writeln!(writer, "${value:04X}#{label}#") {
                    return Err(DebugExporterError::new(format!(
                        "Failed to write to \"{}\": {e}",
                        path.display()
                    )));
                }
            }
        }

        Ok(())
    }
}

use std::{
    cell::RefCell,
    io::{Read, Write},
    path::{Path, PathBuf},
    rc::Rc,
};

use fxhash::FxHashMap;

use crate::{
    fileman::{FileManager, FileSystem},
    intern::StrInterner,
    linker::{DebugExporter, DebugExporterError},
    symtab::{Symbol, Symtab},
};

pub struct NameList<S> {
    file_manager: FileManager<S>,
    str_interner: Rc<RefCell<StrInterner>>,
    cwd: PathBuf,
    path: PathBuf,
}

impl<S, R, W> NameList<S>
where
    S: FileSystem<Reader = R, Writer = W>,
    R: Read,
    W: Write,
{
    pub fn new<C: AsRef<Path>, P: AsRef<Path>>(
        file_manager: FileManager<S>,
        str_interner: Rc<RefCell<StrInterner>>,
        cwd: C,
        path: P,
    ) -> Self {
        let cwd = cwd.as_ref().to_path_buf();
        let path = path.as_ref().to_path_buf();
        Self {
            file_manager,
            str_interner,
            cwd,
            path,
        }
    }
}

impl<S, R, W> DebugExporter for NameList<S>
where
    S: FileSystem<Reader = R, Writer = W>,
    R: Read,
    W: Write,
{
    fn export(&mut self, symtab: &Symtab) -> Result<(), DebugExporterError> {
        let mut ram_entries = Vec::new();
        let mut prg_banks = FxHashMap::default();

        for (strref, _) in symtab.references() {
            let interner = self.str_interner.as_ref().borrow();
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
                let key = interner.get(pair[0]).unwrap();
                let value = interner.get(pair[1]).unwrap();
                match (key, value) {
                    ("ID", "ZP") => ram = true,
                    ("ID", "RAM") => ram = true,
                    ("ID", "PRG") => prg = true,
                    ("BANK", value) => {
                        if let Ok(value) = usize::from_str_radix(value, 10) {
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
                    prg_banks.entry(bank).or_insert_with(Vec::new);
                    prg_banks
                        .get_mut(&bank)
                        .unwrap()
                        .push((strref, value as u16));
                }
            }
        }

        if !ram_entries.is_empty() {
            let mut path = self.path.clone();
            let mut extension = path.extension().unwrap_or_default().to_os_string();
            extension.push(".ram.nl");
            path.set_extension(extension);
            let (_, mut writer) = match self.file_manager.writer(&self.cwd, &path) {
                Ok(tup) => tup,
                Err(e) => {
                    return Err(DebugExporterError::new(format!(
                        "Failed to open \"{}\" for reading: {e}",
                        path.display()
                    )));
                }
            };

            for (label, value) in ram_entries {
                let interner = self.str_interner.as_ref().borrow();
                let label = interner.get(*label).unwrap();
                if let Err(e) = writeln!(writer, "${value:04X}#{label}#") {
                    return Err(DebugExporterError::new(format!(
                        "Failed to write to \"{}\": {e}",
                        path.display()
                    )));
                }
            }
        }

        for (bank, entries) in prg_banks {
            let mut path = self.path.clone();
            let mut extension = path.extension().unwrap_or_default().to_os_string();
            extension.push(format!(".{bank:X}.nl"));
            path.set_extension(extension);
            let (_, mut writer) = match self.file_manager.writer(&self.cwd, &path) {
                Ok(tup) => tup,
                Err(e) => {
                    return Err(DebugExporterError::new(format!(
                        "Failed to open \"{}\" for writing: {e}",
                        path.display()
                    )));
                }
            };

            for (label, value) in entries {
                let interner = self.str_interner.as_ref().borrow();
                let label = interner.get(*label).unwrap();
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

use std::{io::Write, marker::PhantomData, path::Path};

use fxhash::FxHashMap;

use crate::{
    debug::{DebugExporter, DebugExporterError},
    fileman::{FileManager, FileSystem},
    intern::StrInterner,
    symtab::{Symbol, Symtab},
};

pub struct Sym<S> {
    marker: PhantomData<S>,
}

impl<S> Sym<S> {
    #[inline]
    pub fn new() -> Self {
        Self {
            marker: PhantomData,
        }
    }
}

impl<S> DebugExporter for Sym<S>
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
        let mut hram_entries = Vec::new();
        let mut wram_banks = FxHashMap::default();
        let mut sram_banks = FxHashMap::default();
        let mut vram_banks = FxHashMap::default();
        let mut rom_banks = FxHashMap::default();

        for (strref, _) in symtab {
            let sym = symtab.get(*strref).unwrap();
            let value = match sym.inner() {
                Symbol::Value(value) => *value,
                Symbol::Expr(expr) => expr.evaluate(symtab).unwrap(),
            };
            let meta = symtab.meta_interner().get(sym.meta()).unwrap();

            let mut sram = false;
            let mut wram = false;
            let mut vram = false;
            let mut hram = false;
            let mut rom = false;
            let mut bank = None;
            for pair in meta {
                let key = str_interner.get(pair[0]).unwrap();
                let value = str_interner.get(pair[1]).unwrap();
                match (key, value) {
                    ("ID", "WRAM") => wram = true,
                    ("ID", "SRAM") => sram = true,
                    ("ID", "VRAM") => vram = true,
                    ("ID", "HRAM") => hram = true,
                    ("ID", "ROM") => rom = true,
                    ("BANK", value) => {
                        if let Ok(value) = usize::from_str_radix(value, 16) {
                            bank = Some(value);
                        }
                    }
                    _ => {}
                }
            }

            if hram {
                hram_entries.push((strref, value as u16));
            }

            if let Some(bank) = bank {
                if rom {
                    rom_banks
                        .entry(bank)
                        .or_insert_with(Vec::new)
                        .push((strref, value as u16));
                }
                if wram {
                    wram_banks
                        .entry(bank)
                        .or_insert_with(Vec::new)
                        .push((strref, value as u16));
                }
                if sram {
                    sram_banks
                        .entry(bank)
                        .or_insert_with(Vec::new)
                        .push((strref, value as u16));
                }
                if vram {
                    vram_banks
                        .entry(bank)
                        .or_insert_with(Vec::new)
                        .push((strref, value as u16));
                }
            }
        }

        let (_, mut writer) = match file_manager.writer(&cwd, &path) {
            Ok(tup) => tup,
            Err(e) => {
                return Err(DebugExporterError::new(format!(
                    "Failed to open \"{}\" for wrting: {e}",
                    path.display()
                )));
            }
        };

        for (label, value) in hram_entries {
            let label = str_interner.get(*label).unwrap();
            if let Err(e) = writeln!(writer, "{value:04X} {label}") {
                return Err(DebugExporterError::new(format!(
                    "Failed to write to \"{}\": {e}",
                    path.display()
                )));
            }
        }

        for (bank, entries) in rom_banks
            .iter()
            .chain(wram_banks.iter())
            .chain(sram_banks.iter())
            .chain(vram_banks.iter())
        {
            for (&label, value) in entries {
                let label = str_interner.get(label).unwrap();
                if let Err(e) = writeln!(writer, "{bank:X}:{value:04X} {label}") {
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

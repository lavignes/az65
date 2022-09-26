use std::{
    env,
    fs::{self, File},
    io::{self, Write},
    path::PathBuf,
    process::ExitCode,
};

use az65::{
    assembler::Assembler,
    debug::{AZ65Meta, DebugExporter},
    fileman::RealFileSystem,
    mos6502::{Mos6502, NameList},
    sm83::{Sm83, Sym},
    z80::Z80,
};
use clap::{Parser, Subcommand};

/// A multi-CPU assembler
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
#[clap(propagate_version = true)]
#[clap(subcommand_required = true)]
struct Args {
    /// Target architecture
    #[clap(subcommand)]
    architecture: Arch,

    /// Path to output binary file [default: stdout]
    #[clap(parse(from_os_str), short, long, value_name = "FILE")]
    output: Option<PathBuf>,

    /// Paths to search for included files [repeatable]
    #[clap(parse(from_os_str), short = 'I', long, value_name = "DIRECTORY")]
    include: Vec<PathBuf>,

    /// Write AZ65 debug symbols to file
    #[clap(parse(from_os_str), short = 'g', long, value_name = "FILE")]
    debug: Option<PathBuf>,
}

#[derive(Debug, Subcommand)]
enum Arch {
    /// MOS 6502 assembler
    #[clap(name = "6502")]
    Mos6502 {
        /// Path to input assembly file
        #[clap(parse(from_os_str), value_name = "FILE")]
        file: PathBuf,

        /// Base path and file name to output FCEUX (NES Emulator) "NameList" files
        ///
        /// For example, passing `-gNL path/myrom.nes` can generate files such as:
        /// * `path/myrom.nes.ram.nl`
        /// * `path/myrom.nes.0.nl`
        #[clap(
            parse(from_os_str),
            long = "gNL",
            value_name = "BASE_NAME",
            verbatim_doc_comment
        )]
        g_nl: Option<PathBuf>,
    },

    /// Zilog Z80 assembler
    Z80 {
        /// Path to input assembly file
        #[clap(parse(from_os_str), value_name = "FILE")]
        file: PathBuf,
    },

    /// SM83 / GameBoy Z80 assembler
    Sm83 {
        /// Path to input assembly file
        #[clap(parse(from_os_str), value_name = "FILE")]
        file: PathBuf,

        /// Write SYM debug symboks to file
        #[clap(parse(from_os_str), long = "gSYM", value_name = "FILE")]
        g_sym: Option<PathBuf>,
    },
}

fn main() -> ExitCode {
    let args = <Args as Parser>::parse();

    let mut output: Box<dyn Write> = if let Some(path) = args.output {
        let result = File::options()
            .write(true)
            .create(true)
            .truncate(true)
            .open(path.as_path());
        match result {
            Err(e) => {
                eprintln!(
                    "[ERROR]: Cannot open output file \"{}\" for writing: {e}",
                    path.display()
                );
                return ExitCode::FAILURE;
            }
            Ok(file) => Box::new(file),
        }
    } else {
        Box::new(io::stdout())
    };

    let cwd = env::current_dir().unwrap();
    let full_cwd = fs::canonicalize(cwd).unwrap();
    let file_system = RealFileSystem::new();
    let module = match &args.architecture {
        Arch::Mos6502 { file, .. } => {
            let mut assembler = Assembler::new(file_system, Mos6502);
            for path in &args.include {
                if let Err(e) = assembler.add_search_path(full_cwd.as_path(), path) {
                    eprintln!("[ERROR]: {e}");
                    return ExitCode::FAILURE;
                }
            }
            match assembler.assemble(full_cwd.as_path(), file) {
                Ok(module) => module,
                Err(e) => {
                    eprintln!("[ERROR]: {e}");
                    return ExitCode::FAILURE;
                }
            }
        }
        Arch::Z80 { file, .. } => {
            let mut assembler = Assembler::new(file_system, Z80);
            for path in &args.include {
                if let Err(e) = assembler.add_search_path(full_cwd.as_path(), path) {
                    eprintln!("[ERROR]: {e}");
                    return ExitCode::FAILURE;
                }
            }
            match assembler.assemble(full_cwd.as_path(), file) {
                Ok(module) => module,
                Err(e) => {
                    eprintln!("[ERROR]: {e}");
                    return ExitCode::FAILURE;
                }
            }
        }
        Arch::Sm83 { file, .. } => {
            let mut assembler = Assembler::new(file_system, Sm83);
            for path in &args.include {
                if let Err(e) = assembler.add_search_path(full_cwd.as_path(), path) {
                    eprintln!("[ERROR]: {e}");
                    return ExitCode::FAILURE;
                }
            }
            match assembler.assemble(full_cwd.as_path(), file) {
                Ok(module) => module,
                Err(e) => {
                    eprintln!("[ERROR]: {e}");
                    return ExitCode::FAILURE;
                }
            }
        }
    };

    match module.link(&mut output) {
        Ok((str_interner, mut file_manager, symtab)) => {
            if let Arch::Mos6502 {
                g_nl: Some(path), ..
            } = &args.architecture
            {
                let mut nl = NameList::new();
                if let Err(e) = nl.export(
                    &mut file_manager,
                    str_interner.as_ref().borrow(),
                    &symtab,
                    full_cwd.as_path(),
                    &path,
                ) {
                    eprintln!("[ERROR]: {e}");
                    return ExitCode::FAILURE;
                }
            }

            if let Arch::Sm83 {
                g_sym: Some(path), ..
            } = &args.architecture
            {
                let mut sym = Sym::new();
                if let Err(e) = sym.export(
                    &mut file_manager,
                    str_interner.as_ref().borrow(),
                    &symtab,
                    full_cwd.as_path(),
                    &path,
                ) {
                    eprintln!("[ERROR]: {e}");
                    return ExitCode::FAILURE;
                }
            }

            if let Some(path) = &args.debug {
                let mut meta = AZ65Meta::new();
                if let Err(e) = meta.export(
                    &mut file_manager,
                    str_interner.as_ref().borrow(),
                    &symtab,
                    full_cwd.as_path(),
                    &path,
                ) {
                    eprintln!("[ERROR]: {e}");
                    return ExitCode::FAILURE;
                }
            }

            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("[ERROR]: {e}");
            ExitCode::FAILURE
        }
    }
}

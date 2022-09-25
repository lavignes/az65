use std::{
    env,
    fs::{self, File},
    io::{self, Write},
    path::PathBuf,
    process::ExitCode,
};

use az65::{
    assembler::Assembler,
    fileman::RealFileSystem,
    linker::DebugExporter,
    mos6502::{Mos6502, NameList},
    sm83::Sm83,
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
}

#[derive(Debug, Subcommand)]
enum Arch {
    /// MOS 6502 assembler
    #[clap(name = "6502")]
    Mos6502 {
        /// Path to input assembly file
        #[clap(parse(from_os_str), value_name = "FILE")]
        file: PathBuf,

        /// Base path and filename to output FCEUX (NES Emulator) "NameList" files
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
        Arch::Z80 { file } => {
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
        Arch::Sm83 { file } => {
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
        Ok((str_interner, file_system, symtab)) => {
            if let Arch::Mos6502 {
                g_nl: Some(path), ..
            } = &args.architecture
            {
                let mut nl = NameList::new(file_system, str_interner, full_cwd.as_path(), path);
                if let Err(e) = nl.export(&symtab) {
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

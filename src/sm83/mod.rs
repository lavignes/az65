use std::{
    fmt::{self, Display, Formatter},
    io::Read,
};

use crate::{
    assembler::{ArchAssembler, Assembler, AssemblerError},
    fileman::FileSystem,
    lexer::{self, ArchTokens, SourceLoc, SymbolName, Token},
};

#[cfg(test)]
mod tests;

macro_rules! asm_err {
    ($loc:expr, $($arg:tt)*) => {
        Err(($loc, AssemblerError(format!($($arg)*))))
    };
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum OperationName {
    Nop,
    Ld,
    Inc,
    Dec,
    Rlca,
    Add,
    Sub,
    Rrca,
    Stop,
    Rla,
    Jr,
    Jp,
    Rra,
    Daa,
    Cpl,
    Scf,
    Ccf,
    Adc,
    Sbc,
    And,
    Xor,
    Or,
    Cp,
    Ret,
    Pop,
    Call,
    Push,
    Rst,
    Reti,
    Ldh,
    Di,
    Ei,
    Rlc,
    Rrc,
    Rl,
    Rr,
    Sla,
    Sra,
    Swap,
    Srl,
    Bit,
    Res,
    Set,
    Halt,
}

impl lexer::OperationName for OperationName {
    fn parse<S: AsRef<str>>(s: S) -> Option<Self> {
        match s.as_ref() {
            "NOP" | "nop" => Some(Self::Nop),
            "LD" | "ld" => Some(Self::Ld),
            "INC" | "inc" => Some(Self::Inc),
            "DEC" | "dec" => Some(Self::Dec),
            "RLCA" | "rlca" => Some(Self::Rlca),
            "ADD" | "add" => Some(Self::Add),
            "SUB" | "sub" => Some(Self::Sub),
            "RRCA" | "rrca" => Some(Self::Rrca),
            "STOP" | "stop" => Some(Self::Stop),
            "RLA" | "rla" => Some(Self::Rla),
            "JR" | "jr" => Some(Self::Jr),
            "JP" | "jp" => Some(Self::Jp),
            "RRA" | "rra" => Some(Self::Rra),
            "DAA" | "daa" => Some(Self::Daa),
            "CPL" | "cpl" => Some(Self::Cpl),
            "SCF" | "scf" => Some(Self::Scf),
            "CCF" | "ccf" => Some(Self::Ccf),
            "ADC" | "adc" => Some(Self::Adc),
            "SBC" | "sbc" => Some(Self::Sbc),
            "AND" | "and" => Some(Self::And),
            "XOR" | "xor" => Some(Self::Xor),
            "OR" | "or" => Some(Self::Or),
            "CP" | "cp" => Some(Self::Cp),
            "RET" | "ret" => Some(Self::Ret),
            "POP" | "pop" => Some(Self::Pop),
            "CALL" | "call" => Some(Self::Call),
            "PUSH" | "push" => Some(Self::Push),
            "RST" | "rst" => Some(Self::Rst),
            "RETI" | "reti" => Some(Self::Reti),
            "LDH" | "ldh" => Some(Self::Ldh),
            "DI" | "di" => Some(Self::Di),
            "EI" | "ei" => Some(Self::Ei),
            "RLC" | "rlc" => Some(Self::Rlc),
            "RRC" | "rrc" => Some(Self::Rrc),
            "RL" | "rl" => Some(Self::Rl),
            "RR" | "rr" => Some(Self::Rr),
            "SLA" | "sla" => Some(Self::Sla),
            "SRA" | "sra" => Some(Self::Sra),
            "SWAP" | "swap" => Some(Self::Swap),
            "SRL" | "srl" => Some(Self::Srl),
            "BIT" | "bit" => Some(Self::Bit),
            "RES" | "res" => Some(Self::Res),
            "SET" | "set" => Some(Self::Set),
            "HALT" | "halt" => Some(Self::Halt),
            _ => None,
        }
    }
}

impl Display for OperationName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Nop => "nop",
                Self::Ld => "ld",
                Self::Inc => "inc",
                Self::Dec => "dec",
                Self::Rlca => "rlca",
                Self::Add => "add",
                Self::Sub => "sub",
                Self::Rrca => "rrca",
                Self::Stop => "stop",
                Self::Rla => "rla",
                Self::Jr => "jr",
                Self::Jp => "jp",
                Self::Rra => "rra",
                Self::Daa => "daa",
                Self::Cpl => "cpl",
                Self::Scf => "scf",
                Self::Ccf => "ccf",
                Self::Adc => "adc",
                Self::Sbc => "sbc",
                Self::And => "and",
                Self::Xor => "xor",
                Self::Or => "or",
                Self::Cp => "cp",
                Self::Ret => "ret",
                Self::Pop => "pop",
                Self::Call => "call",
                Self::Push => "push",
                Self::Rst => "rst",
                Self::Reti => "reti",
                Self::Ldh => "ldh",
                Self::Di => "di",
                Self::Ei => "ei",
                Self::Rlc => "rlc",
                Self::Rrc => "rrc",
                Self::Rl => "rl",
                Self::Rr => "rr",
                Self::Sla => "sla",
                Self::Sra => "sra",
                Self::Swap => "swap",
                Self::Srl => "srl",
                Self::Bit => "bit",
                Self::Res => "res",
                Self::Set => "set",
                Self::Halt => "halt",
            }
        )
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum FlagName {
    Z,
    C,
    NZ,
    NC,
}

impl lexer::FlagName for FlagName {
    fn parse<S: AsRef<str>>(s: S) -> Option<Self> {
        match s.as_ref() {
            "Z" | "z" => Some(Self::Z),
            "C" | "c" => Some(Self::C),
            "NZ" | "nz" => Some(Self::NZ),
            "NC" | "nc" => Some(Self::NC),
            _ => None,
        }
    }
}

impl Display for FlagName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Z => "z",
                Self::C => "c",
                Self::NZ => "nz",
                Self::NC => "nc",
            }
        )
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum RegisterName {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    AF,
    BC,
    DE,
    HL,
    PC,
    SP,
}

impl lexer::RegisterName for RegisterName {
    fn parse<S: AsRef<str>>(s: S) -> Option<Self> {
        match s.as_ref() {
            "A" | "a" => Some(Self::A),
            "B" | "b" => Some(Self::B),
            "C" | "c" => Some(Self::C),
            "D" | "d" => Some(Self::D),
            "E" | "e" => Some(Self::E),
            "H" | "h" => Some(Self::H),
            "L" | "l" => Some(Self::L),
            "AF" | "af" => Some(Self::AF),
            "BC" | "bc" => Some(Self::BC),
            "DE" | "de" => Some(Self::DE),
            "HL" | "hl" => Some(Self::HL),
            "PC" | "pc" => Some(Self::PC),
            "SP" | "sp" => Some(Self::SP),
            _ => None,
        }
    }
}

impl Display for RegisterName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::A => "a",
                Self::B => "b",
                Self::C => "c",
                Self::D => "d",
                Self::E => "e",
                Self::H => "h",
                Self::L => "l",
                Self::AF => "af",
                Self::BC => "bc",
                Self::DE => "de",
                Self::HL => "hl",
                Self::PC => "pc",
                Self::SP => "sp",
            }
        )
    }
}

pub struct Sm83;

#[derive(Copy, Clone)]
pub struct Sm83Tokens;

impl ArchTokens for Sm83Tokens {
    type RegisterName = RegisterName;
    type FlagName = FlagName;
    type OperationName = OperationName;
}

impl<S, R> ArchAssembler<S, R, Sm83Tokens> for Sm83
where
    S: FileSystem<Reader = R>,
    R: Read,
{
    fn parse(
        asm: &mut Assembler<S, R, Sm83Tokens, Self>,
        name: OperationName,
    ) -> Result<(), (SourceLoc, AssemblerError)> {
        match name {
            OperationName::Nop => {
                asm.next()?;
                asm.data.push(0x00);
            }

            OperationName::Ld => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register {
                        name: RegisterName::BC,
                        ..
                    }) => {
                        asm.data.push(0x01);
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.expect_wide_immediate()?;
                    }

                    Some(Token::Register {
                        name: RegisterName::DE,
                        ..
                    }) => {
                        asm.data.push(0x11);
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.expect_wide_immediate()?;
                    }

                    Some(Token::Register {
                        name: RegisterName::HL,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        match asm.peek()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Register {
                                name: RegisterName::SP,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xF8);
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.expect_immediate()?;
                            }

                            Some(_) => {
                                asm.data.push(0x21);
                                asm.expect_wide_immediate()?;
                            }
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::SP,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        match asm.peek()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Register {
                                name: RegisterName::HL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xF9);
                            }

                            Some(_) => {
                                asm.data.push(0x31);
                                asm.expect_wide_immediate()?;
                            }
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        match asm.peek()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Register {
                                name: RegisterName::A,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x7F);
                            }

                            Some(Token::Register {
                                name: RegisterName::B,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x78);
                            }

                            Some(Token::Register {
                                name: RegisterName::C,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x79);
                            }

                            Some(Token::Register {
                                name: RegisterName::D,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x7A);
                            }

                            Some(Token::Register {
                                name: RegisterName::E,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x7B);
                            }

                            Some(Token::Register {
                                name: RegisterName::H,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x7C);
                            }

                            Some(Token::Register {
                                name: RegisterName::L,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x7D);
                            }

                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            }) => {
                                asm.next()?;
                                match asm.peek()? {
                                    None => return asm.end_of_input_err(),

                                    Some(Token::Register {
                                        name: RegisterName::BC,
                                        ..
                                    }) => {
                                        asm.next()?;
                                        asm.expect_symbol(SymbolName::ParenClose)?;
                                        asm.data.push(0x0A);
                                    }

                                    Some(Token::Register {
                                        name: RegisterName::DE,
                                        ..
                                    }) => {
                                        asm.next()?;
                                        asm.expect_symbol(SymbolName::ParenClose)?;
                                        asm.data.push(0x1A);
                                    }

                                    Some(Token::Register {
                                        name: RegisterName::C,
                                        ..
                                    }) => {
                                        asm.next()?;
                                        asm.expect_symbol(SymbolName::ParenClose)?;
                                        asm.data.push(0xF2);
                                    }

                                    Some(Token::Register {
                                        name: RegisterName::HL,
                                        ..
                                    }) => {
                                        asm.next()?;
                                        match asm.peek()? {
                                            None => return asm.end_of_input_err(),

                                            Some(Token::Symbol {
                                                name: SymbolName::Plus,
                                                ..
                                            }) => {
                                                asm.next()?;
                                                asm.expect_symbol(SymbolName::ParenClose)?;
                                                asm.data.push(0x2A);
                                            }

                                            Some(Token::Symbol {
                                                name: SymbolName::Minus,
                                                ..
                                            }) => {
                                                asm.next()?;
                                                asm.expect_symbol(SymbolName::ParenClose)?;
                                                asm.data.push(0x3A);
                                            }

                                            Some(_) => {
                                                asm.expect_symbol(SymbolName::ParenClose)?;
                                                asm.data.push(0x7E);
                                            }
                                        }
                                    }

                                    Some(_) => {
                                        asm.data.push(0xFA);
                                        asm.expect_wide_immediate()?;
                                        asm.expect_symbol(SymbolName::ParenClose)?;
                                    }
                                }
                            }

                            Some(_) => {
                                asm.data.push(0x3E);
                                asm.expect_immediate()?;
                            }
                        }
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => match asm.peek()? {
                        None => return asm.end_of_input_err(),

                        Some(Token::Register {
                            name: RegisterName::BC,
                            ..
                        }) => {
                            asm.next()?;
                            asm.expect_symbol(SymbolName::ParenClose)?;
                            asm.expect_symbol(SymbolName::Comma)?;
                            asm.expect_register(RegisterName::A)?;
                            asm.data.push(0x02);
                        }

                        Some(Token::Register {
                            name: RegisterName::DE,
                            ..
                        }) => {
                            asm.next()?;
                            asm.expect_symbol(SymbolName::ParenClose)?;
                            asm.expect_symbol(SymbolName::Comma)?;
                            asm.expect_register(RegisterName::A)?;
                            asm.data.push(0x12);
                        }

                        Some(Token::Register {
                            name: RegisterName::C,
                            ..
                        }) => {
                            asm.next()?;
                            asm.expect_symbol(SymbolName::ParenClose)?;
                            asm.expect_symbol(SymbolName::Comma)?;
                            asm.expect_register(RegisterName::A)?;
                            asm.data.push(0xE2);
                        }

                        Some(Token::Register {
                            name: RegisterName::HL,
                            ..
                        }) => {
                            asm.next()?;

                            match asm.peek()? {
                                None => return asm.end_of_input_err(),

                                Some(Token::Symbol {
                                    name: SymbolName::Plus,
                                    ..
                                }) => {
                                    asm.next()?;
                                    asm.expect_symbol(SymbolName::ParenClose)?;
                                    asm.expect_symbol(SymbolName::Comma)?;
                                    asm.expect_register(RegisterName::A)?;
                                    asm.data.push(0x22);
                                }

                                Some(Token::Symbol {
                                    name: SymbolName::Minus,
                                    ..
                                }) => {
                                    asm.next()?;
                                    asm.expect_symbol(SymbolName::ParenClose)?;
                                    asm.expect_symbol(SymbolName::Comma)?;
                                    asm.expect_register(RegisterName::A)?;
                                    asm.data.push(0x32);
                                }

                                Some(_) => {
                                    asm.expect_symbol(SymbolName::ParenClose)?;
                                    asm.expect_symbol(SymbolName::Comma)?;
                                    match asm.peek()? {
                                        None => return asm.end_of_input_err(),

                                        Some(Token::Register {
                                            name: RegisterName::A,
                                            ..
                                        }) => {
                                            asm.next()?;
                                            asm.data.push(0x77);
                                        }

                                        Some(Token::Register {
                                            name: RegisterName::B,
                                            ..
                                        }) => {
                                            asm.next()?;
                                            asm.data.push(0x70);
                                        }

                                        Some(Token::Register {
                                            name: RegisterName::C,
                                            ..
                                        }) => {
                                            asm.next()?;
                                            asm.data.push(0x71);
                                        }

                                        Some(Token::Register {
                                            name: RegisterName::D,
                                            ..
                                        }) => {
                                            asm.next()?;
                                            asm.data.push(0x72);
                                        }

                                        Some(Token::Register {
                                            name: RegisterName::E,
                                            ..
                                        }) => {
                                            asm.next()?;
                                            asm.data.push(0x73);
                                        }

                                        Some(Token::Register {
                                            name: RegisterName::H,
                                            ..
                                        }) => {
                                            asm.next()?;
                                            asm.data.push(0x74);
                                        }

                                        Some(Token::Register {
                                            name: RegisterName::L,
                                            ..
                                        }) => {
                                            asm.next()?;
                                            asm.data.push(0x75);
                                        }

                                        Some(_) => {
                                            asm.data.push(0x36);
                                            asm.expect_immediate()?;
                                        }
                                    }
                                }
                            }
                        }

                        Some(_) => {
                            let opcode_index = asm.data.len();
                            asm.data.push(0);
                            asm.expect_wide_immediate()?;
                            asm.expect_symbol(SymbolName::ParenClose)?;
                            asm.expect_symbol(SymbolName::Comma)?;
                            match asm.next()? {
                                None => return asm.end_of_input_err(),

                                Some(Token::Register {
                                    name: RegisterName::A,
                                    ..
                                }) => {
                                    asm.data[opcode_index] = 0xEA;
                                }

                                Some(Token::Register {
                                    name: RegisterName::SP,
                                    ..
                                }) => {
                                    asm.data[opcode_index] = 0x08;
                                }

                                Some(tok) => {
                                    return asm_err!(
                                        tok.loc(),
                                        "Unexpected {}, expected register \"a\" or \"sp\"",
                                        tok.as_display(&asm.str_interner)
                                    )
                                }
                            }
                        }
                    },

                    Some(Token::Register {
                        name: RegisterName::B,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        match asm.peek()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Register {
                                name: RegisterName::A,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x47);
                            }

                            Some(Token::Register {
                                name: RegisterName::B,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x40);
                            }

                            Some(Token::Register {
                                name: RegisterName::C,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x41);
                            }

                            Some(Token::Register {
                                name: RegisterName::D,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x42);
                            }

                            Some(Token::Register {
                                name: RegisterName::E,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x43);
                            }

                            Some(Token::Register {
                                name: RegisterName::H,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x44);
                            }

                            Some(Token::Register {
                                name: RegisterName::L,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x45);
                            }

                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            }) => {
                                asm.next()?;
                                asm.expect_register(RegisterName::HL)?;
                                asm.expect_symbol(SymbolName::ParenClose)?;
                                asm.data.push(0x46);
                            }

                            Some(_) => {
                                asm.data.push(0x06);
                                asm.expect_immediate()?;
                            }
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        match asm.peek()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Register {
                                name: RegisterName::A,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x4F);
                            }

                            Some(Token::Register {
                                name: RegisterName::B,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x48);
                            }

                            Some(Token::Register {
                                name: RegisterName::C,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x49);
                            }

                            Some(Token::Register {
                                name: RegisterName::D,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x4A);
                            }

                            Some(Token::Register {
                                name: RegisterName::E,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x4B);
                            }

                            Some(Token::Register {
                                name: RegisterName::H,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x4C);
                            }

                            Some(Token::Register {
                                name: RegisterName::L,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x4D);
                            }

                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            }) => {
                                asm.next()?;
                                asm.expect_register(RegisterName::HL)?;
                                asm.expect_symbol(SymbolName::ParenClose)?;
                                asm.data.push(0x4E);
                            }

                            Some(_) => {
                                asm.data.push(0x0E);
                                asm.expect_immediate()?;
                            }
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::D,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        match asm.peek()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Register {
                                name: RegisterName::A,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x57);
                            }

                            Some(Token::Register {
                                name: RegisterName::B,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x50);
                            }

                            Some(Token::Register {
                                name: RegisterName::C,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x51);
                            }

                            Some(Token::Register {
                                name: RegisterName::D,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x52);
                            }

                            Some(Token::Register {
                                name: RegisterName::E,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x53);
                            }

                            Some(Token::Register {
                                name: RegisterName::H,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x54);
                            }

                            Some(Token::Register {
                                name: RegisterName::L,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x55);
                            }

                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            }) => {
                                asm.next()?;
                                asm.expect_register(RegisterName::HL)?;
                                asm.expect_symbol(SymbolName::ParenClose)?;
                                asm.data.push(0x56);
                            }

                            Some(_) => {
                                asm.data.push(0x16);
                                asm.expect_immediate()?;
                            }
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::E,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        match asm.peek()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Register {
                                name: RegisterName::A,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x5F);
                            }

                            Some(Token::Register {
                                name: RegisterName::B,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x58);
                            }

                            Some(Token::Register {
                                name: RegisterName::C,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x59);
                            }

                            Some(Token::Register {
                                name: RegisterName::D,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x5A);
                            }

                            Some(Token::Register {
                                name: RegisterName::E,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x5B);
                            }

                            Some(Token::Register {
                                name: RegisterName::H,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x5C);
                            }

                            Some(Token::Register {
                                name: RegisterName::L,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x5D);
                            }

                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            }) => {
                                asm.next()?;
                                asm.expect_register(RegisterName::HL)?;
                                asm.expect_symbol(SymbolName::ParenClose)?;
                                asm.data.push(0x5E);
                            }

                            Some(_) => {
                                asm.data.push(0x1E);
                                asm.expect_immediate()?;
                            }
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::H,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        match asm.peek()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Register {
                                name: RegisterName::A,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x67);
                            }

                            Some(Token::Register {
                                name: RegisterName::B,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x60);
                            }

                            Some(Token::Register {
                                name: RegisterName::C,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x61);
                            }

                            Some(Token::Register {
                                name: RegisterName::D,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x62);
                            }

                            Some(Token::Register {
                                name: RegisterName::E,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x63);
                            }

                            Some(Token::Register {
                                name: RegisterName::H,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x64);
                            }

                            Some(Token::Register {
                                name: RegisterName::L,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x65);
                            }

                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            }) => {
                                asm.next()?;
                                asm.expect_register(RegisterName::HL)?;
                                asm.expect_symbol(SymbolName::ParenClose)?;
                                asm.data.push(0x66);
                            }

                            Some(_) => {
                                asm.data.push(0x26);
                                asm.expect_immediate()?;
                            }
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::L,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        match asm.peek()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Register {
                                name: RegisterName::A,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x6F);
                            }

                            Some(Token::Register {
                                name: RegisterName::B,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x68);
                            }

                            Some(Token::Register {
                                name: RegisterName::C,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x69);
                            }

                            Some(Token::Register {
                                name: RegisterName::D,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x6A);
                            }

                            Some(Token::Register {
                                name: RegisterName::E,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x6B);
                            }

                            Some(Token::Register {
                                name: RegisterName::H,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x6C);
                            }

                            Some(Token::Register {
                                name: RegisterName::L,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x6D);
                            }

                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            }) => {
                                asm.next()?;
                                asm.expect_register(RegisterName::HL)?;
                                asm.expect_symbol(SymbolName::ParenClose)?;
                                asm.data.push(0x6E);
                            }

                            Some(_) => {
                                asm.data.push(0x2E);
                                asm.expect_immediate()?;
                            }
                        }
                    }
                    Some(tok) => {
                        return asm_err!(
                        tok.loc(),
                        "Unexpected {}, expected a valid destination register or indirect address",
                        tok.as_display(&asm.str_interner)
                    )
                    }
                }
            }

            OperationName::Ldh => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.data.push(0xE0);
                        asm.expect_hmem_immediate()?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.expect_register(RegisterName::A)?;
                    }

                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.data.push(0xF0);
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.expect_symbol(SymbolName::ParenOpen)?;
                        asm.expect_hmem_immediate()?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                    }

                    Some(tok) => {
                        return asm_err!(
                        tok.loc(),
                        "Unexpected {}, expected the \"a\" register or high memory page indirect address",
                        tok.as_display(&asm.str_interner)
                    )
                    }
                }
            }

            OperationName::Stop => {
                asm.next()?;
                asm.data.push(0x10);
                asm.data.push(0x00);
            }

            OperationName::Jr => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Flag {
                        name: FlagName::NZ, ..
                    }) => {
                        asm.next()?;
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.data.push(0x20);
                        asm.expect_branch_immediate()?;
                    }

                    Some(Token::Flag {
                        name: FlagName::NC, ..
                    }) => {
                        asm.next()?;
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.data.push(0x30);
                        asm.expect_branch_immediate()?;
                    }

                    Some(Token::Flag {
                        name: FlagName::Z, ..
                    }) => {
                        asm.next()?;
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.data.push(0x28);
                        asm.expect_branch_immediate()?;
                    }

                    Some(Token::Flag {
                        name: FlagName::C, ..
                    }
                    | Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.next()?;
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.data.push(0x38);
                        asm.expect_branch_immediate()?;
                    }

                    Some(_) => {
                        asm.data.push(0x18);
                        asm.expect_branch_immediate()?;
                    }
                }
            }

            OperationName::Inc => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register {
                        name: RegisterName::BC,
                        ..
                    }) => {
                        asm.data.push(0x03);
                    }

                    Some(Token::Register {
                        name: RegisterName::DE,
                        ..
                    }) => {
                        asm.data.push(0x13);
                    }

                    Some(Token::Register {
                        name: RegisterName::HL,
                        ..
                    }) => {
                        asm.data.push(0x23);
                    }

                    Some(Token::Register {
                        name: RegisterName::SP,
                        ..
                    }) => {
                        asm.data.push(0x33);
                    }

                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.data.push(0x3C);
                    }

                    Some(Token::Register {
                        name: RegisterName::B,
                        ..
                    }) => {
                        asm.data.push(0x04);
                    }

                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.data.push(0x0C);
                    }

                    Some(Token::Register {
                        name: RegisterName::D,
                        ..
                    }) => {
                        asm.data.push(0x14);
                    }

                    Some(Token::Register {
                        name: RegisterName::E,
                        ..
                    }) => {
                        asm.data.push(0x1C);
                    }

                    Some(Token::Register {
                        name: RegisterName::H,
                        ..
                    }) => {
                        asm.data.push(0x24);
                    }

                    Some(Token::Register {
                        name: RegisterName::L,
                        ..
                    }) => {
                        asm.data.push(0x2C);
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.data.push(0x34);
                        asm.expect_register(RegisterName::HL)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                    }

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected a register or register pair",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                }
            }

            OperationName::Dec => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register {
                        name: RegisterName::BC,
                        ..
                    }) => {
                        asm.data.push(0x0B);
                    }

                    Some(Token::Register {
                        name: RegisterName::DE,
                        ..
                    }) => {
                        asm.data.push(0x1B);
                    }

                    Some(Token::Register {
                        name: RegisterName::HL,
                        ..
                    }) => {
                        asm.data.push(0x2B);
                    }

                    Some(Token::Register {
                        name: RegisterName::SP,
                        ..
                    }) => {
                        asm.data.push(0x3B);
                    }

                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.data.push(0x3D);
                    }

                    Some(Token::Register {
                        name: RegisterName::B,
                        ..
                    }) => {
                        asm.data.push(0x05);
                    }

                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.data.push(0x0D);
                    }

                    Some(Token::Register {
                        name: RegisterName::D,
                        ..
                    }) => {
                        asm.data.push(0x15);
                    }

                    Some(Token::Register {
                        name: RegisterName::E,
                        ..
                    }) => {
                        asm.data.push(0x1D);
                    }

                    Some(Token::Register {
                        name: RegisterName::H,
                        ..
                    }) => {
                        asm.data.push(0x25);
                    }

                    Some(Token::Register {
                        name: RegisterName::L,
                        ..
                    }) => {
                        asm.data.push(0x2D);
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.data.push(0x35);
                        asm.expect_register(RegisterName::HL)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                    }

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected a register or register pair",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                }
            }

            OperationName::Rlca => {
                asm.next()?;
                asm.data.push(0x07);
            }

            OperationName::Rrca => {
                asm.next()?;
                asm.data.push(0x0F);
            }

            OperationName::Rla => {
                asm.next()?;
                asm.data.push(0x17);
            }

            OperationName::Rra => {
                asm.next()?;
                asm.data.push(0x1F);
            }

            OperationName::Daa => {
                asm.next()?;
                asm.data.push(0x27);
            }

            OperationName::Scf => {
                asm.next()?;
                asm.data.push(0x37);
            }

            OperationName::Cpl => {
                asm.next()?;
                asm.data.push(0x2F);
            }

            OperationName::Ccf => {
                asm.next()?;
                asm.data.push(0x3F);
            }

            OperationName::Halt => {
                asm.next()?;
                asm.data.push(0x76);
            }

            OperationName::Add => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register { name: RegisterName::HL, .. }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        match asm.next()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Register { name: RegisterName::BC, .. }) => {
                                asm.data.push(0x09);
                            }

                            Some(Token::Register { name: RegisterName::DE, .. }) => {
                                asm.data.push(0x19);
                            }
                            
                            Some(Token::Register { name: RegisterName::HL, .. }) => {
                                asm.data.push(0x29);
                            }
                            
                            Some(Token::Register { name: RegisterName::SP, .. }) => {
                                asm.data.push(0x039);
                            }

                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected register \"bc\", \"de\", \"hl\", or \"sp\"",
                                    tok.as_display(&asm.str_interner)
                                )
                            }
                        }
                    }

                    Some(Token::Register { name: RegisterName::SP, .. }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.data.push(0xE8);
                        asm.expect_immediate()?;
                    }

                    Some(Token::Register { name: RegisterName::A, .. }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        match asm.peek()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Register { name: RegisterName::A, .. }) => {
                                asm.next()?;
                                asm.data.push(0x87);
                            }
                            
                            Some(Token::Register { name: RegisterName::B, .. }) => {
                                asm.next()?;
                                asm.data.push(0x80);
                            }

                            Some(Token::Register { name: RegisterName::C, .. }) => {
                                asm.next()?;
                                asm.data.push(0x81);
                            }
                            
                            Some(Token::Register { name: RegisterName::D, .. }) => {
                                asm.next()?;
                                asm.data.push(0x82);
                            }
                            
                            Some(Token::Register { name: RegisterName::E, .. }) => {
                                asm.next()?;
                                asm.data.push(0x83);
                            }

                            Some(Token::Register { name: RegisterName::H, .. }) => {
                                asm.next()?;
                                asm.data.push(0x84);
                            }
                            
                            Some(Token::Register { name: RegisterName::L, .. }) => {
                                asm.next()?;
                                asm.data.push(0x85);
                            }

                            Some(Token::Symbol { name: SymbolName::ParenOpen, .. }) => {
                                asm.next()?;
                                asm.expect_register(RegisterName::HL)?;
                                asm.expect_symbol(SymbolName::ParenClose)?;
                                asm.data.push(0x86);
                            }

                            Some(_) => {
                                asm.data.push(0xC6);
                                asm.expect_immediate()?;
                            }
                        }
                    }

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected register \"a\", \"hl\", or \"sp\"",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                }
            }

            OperationName::Adc => {
                asm.next()?;
                asm.expect_register(RegisterName::A)?;
                asm.expect_symbol(SymbolName::Comma)?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register { name: RegisterName::A, .. }) => {
                        asm.next()?;
                        asm.data.push(0x8F);
                    }
                    
                    Some(Token::Register { name: RegisterName::B, .. }) => {
                        asm.next()?;
                        asm.data.push(0x88);
                    }

                    Some(Token::Register { name: RegisterName::C, .. }) => {
                        asm.next()?;
                        asm.data.push(0x89);
                    }
                    
                    Some(Token::Register { name: RegisterName::D, .. }) => {
                        asm.next()?;
                        asm.data.push(0x8A);
                    }
                    
                    Some(Token::Register { name: RegisterName::E, .. }) => {
                        asm.next()?;
                        asm.data.push(0x8B);
                    }

                    Some(Token::Register { name: RegisterName::H, .. }) => {
                        asm.next()?;
                        asm.data.push(0x8C);
                    }
                    
                    Some(Token::Register { name: RegisterName::L, .. }) => {
                        asm.next()?;
                        asm.data.push(0x8D);
                    }

                    Some(Token::Symbol { name: SymbolName::ParenOpen, .. }) => {
                        asm.next()?;
                        asm.expect_register(RegisterName::HL)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                        asm.data.push(0x8E);
                    }

                    Some(_) => {
                        asm.data.push(0xCE);
                        asm.expect_immediate()?;
                    }
                }
            }

            OperationName::Sub => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register { name: RegisterName::A, .. }) => {
                        asm.next()?;
                        asm.data.push(0x97);
                    }
                    
                    Some(Token::Register { name: RegisterName::B, .. }) => {
                        asm.next()?;
                        asm.data.push(0x90);
                    }

                    Some(Token::Register { name: RegisterName::C, .. }) => {
                        asm.next()?;
                        asm.data.push(0x91);
                    }
                    
                    Some(Token::Register { name: RegisterName::D, .. }) => {
                        asm.next()?;
                        asm.data.push(0x92);
                    }
                    
                    Some(Token::Register { name: RegisterName::E, .. }) => {
                        asm.next()?;
                        asm.data.push(0x93);
                    }

                    Some(Token::Register { name: RegisterName::H, .. }) => {
                        asm.next()?;
                        asm.data.push(0x94);
                    }
                    
                    Some(Token::Register { name: RegisterName::L, .. }) => {
                        asm.next()?;
                        asm.data.push(0x95);
                    }

                    Some(Token::Symbol { name: SymbolName::ParenOpen, .. }) => {
                        asm.next()?;
                        asm.expect_register(RegisterName::HL)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                        asm.data.push(0x96);
                    }

                    Some(_) => {
                        asm.data.push(0xD6);
                        asm.expect_immediate()?;
                    }
                }
            }
            
            OperationName::Sbc => {
                asm.next()?;
                asm.expect_register(RegisterName::A)?;
                asm.expect_symbol(SymbolName::Comma)?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register { name: RegisterName::A, .. }) => {
                        asm.next()?;
                        asm.data.push(0x9F);
                    }
                    
                    Some(Token::Register { name: RegisterName::B, .. }) => {
                        asm.next()?;
                        asm.data.push(0x98);
                    }

                    Some(Token::Register { name: RegisterName::C, .. }) => {
                        asm.next()?;
                        asm.data.push(0x99);
                    }
                    
                    Some(Token::Register { name: RegisterName::D, .. }) => {
                        asm.next()?;
                        asm.data.push(0x9A);
                    }
                    
                    Some(Token::Register { name: RegisterName::E, .. }) => {
                        asm.next()?;
                        asm.data.push(0x9B);
                    }

                    Some(Token::Register { name: RegisterName::H, .. }) => {
                        asm.next()?;
                        asm.data.push(0x9C);
                    }
                    
                    Some(Token::Register { name: RegisterName::L, .. }) => {
                        asm.next()?;
                        asm.data.push(0x9D);
                    }

                    Some(Token::Symbol { name: SymbolName::ParenOpen, .. }) => {
                        asm.next()?;
                        asm.expect_register(RegisterName::HL)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                        asm.data.push(0x9E);
                    }

                    Some(_) => {
                        asm.data.push(0xDE);
                        asm.expect_immediate()?;
                    }
                }
            }
            
            OperationName::And => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register { name: RegisterName::A, .. }) => {
                        asm.next()?;
                        asm.data.push(0xA7);
                    }
                    
                    Some(Token::Register { name: RegisterName::B, .. }) => {
                        asm.next()?;
                        asm.data.push(0xA0);
                    }

                    Some(Token::Register { name: RegisterName::C, .. }) => {
                        asm.next()?;
                        asm.data.push(0xA1);
                    }
                    
                    Some(Token::Register { name: RegisterName::D, .. }) => {
                        asm.next()?;
                        asm.data.push(0xA2);
                    }
                    
                    Some(Token::Register { name: RegisterName::E, .. }) => {
                        asm.next()?;
                        asm.data.push(0xA3);
                    }

                    Some(Token::Register { name: RegisterName::H, .. }) => {
                        asm.next()?;
                        asm.data.push(0xA4);
                    }
                    
                    Some(Token::Register { name: RegisterName::L, .. }) => {
                        asm.next()?;
                        asm.data.push(0xA5);
                    }

                    Some(Token::Symbol { name: SymbolName::ParenOpen, .. }) => {
                        asm.next()?;
                        asm.expect_register(RegisterName::HL)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                        asm.data.push(0xA6);
                    }

                    Some(_) => {
                        asm.data.push(0xE6);
                        asm.expect_immediate()?;
                    }
                }
            }

            OperationName::Xor => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register { name: RegisterName::A, .. }) => {
                        asm.next()?;
                        asm.data.push(0xAF);
                    }
                    
                    Some(Token::Register { name: RegisterName::B, .. }) => {
                        asm.next()?;
                        asm.data.push(0xA8);
                    }

                    Some(Token::Register { name: RegisterName::C, .. }) => {
                        asm.next()?;
                        asm.data.push(0xA9);
                    }
                    
                    Some(Token::Register { name: RegisterName::D, .. }) => {
                        asm.next()?;
                        asm.data.push(0xAA);
                    }
                    
                    Some(Token::Register { name: RegisterName::E, .. }) => {
                        asm.next()?;
                        asm.data.push(0xAB);
                    }

                    Some(Token::Register { name: RegisterName::H, .. }) => {
                        asm.next()?;
                        asm.data.push(0xAC);
                    }
                    
                    Some(Token::Register { name: RegisterName::L, .. }) => {
                        asm.next()?;
                        asm.data.push(0xAD);
                    }

                    Some(Token::Symbol { name: SymbolName::ParenOpen, .. }) => {
                        asm.next()?;
                        asm.expect_register(RegisterName::HL)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                        asm.data.push(0xAE);
                    }

                    Some(_) => {
                        asm.data.push(0xEE);
                        asm.expect_immediate()?;
                    }
                }
            }

            OperationName::Or => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register { name: RegisterName::A, .. }) => {
                        asm.next()?;
                        asm.data.push(0xB7);
                    }
                    
                    Some(Token::Register { name: RegisterName::B, .. }) => {
                        asm.next()?;
                        asm.data.push(0xB0);
                    }

                    Some(Token::Register { name: RegisterName::C, .. }) => {
                        asm.next()?;
                        asm.data.push(0xB1);
                    }
                    
                    Some(Token::Register { name: RegisterName::D, .. }) => {
                        asm.next()?;
                        asm.data.push(0xB2);
                    }
                    
                    Some(Token::Register { name: RegisterName::E, .. }) => {
                        asm.next()?;
                        asm.data.push(0xB3);
                    }

                    Some(Token::Register { name: RegisterName::H, .. }) => {
                        asm.next()?;
                        asm.data.push(0xB4);
                    }
                    
                    Some(Token::Register { name: RegisterName::L, .. }) => {
                        asm.next()?;
                        asm.data.push(0xB5);
                    }

                    Some(Token::Symbol { name: SymbolName::ParenOpen, .. }) => {
                        asm.next()?;
                        asm.expect_register(RegisterName::HL)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                        asm.data.push(0xB6);
                    }

                    Some(_) => {
                        asm.data.push(0xF6);
                        asm.expect_immediate()?;
                    }
                }
            }

            OperationName::Cp => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register { name: RegisterName::A, .. }) => {
                        asm.next()?;
                        asm.data.push(0xCF);
                    }
                    
                    Some(Token::Register { name: RegisterName::B, .. }) => {
                        asm.next()?;
                        asm.data.push(0xC8);
                    }

                    Some(Token::Register { name: RegisterName::C, .. }) => {
                        asm.next()?;
                        asm.data.push(0xC9);
                    }
                    
                    Some(Token::Register { name: RegisterName::D, .. }) => {
                        asm.next()?;
                        asm.data.push(0xCA);
                    }
                    
                    Some(Token::Register { name: RegisterName::E, .. }) => {
                        asm.next()?;
                        asm.data.push(0xCB);
                    }

                    Some(Token::Register { name: RegisterName::H, .. }) => {
                        asm.next()?;
                        asm.data.push(0xCC);
                    }
                    
                    Some(Token::Register { name: RegisterName::L, .. }) => {
                        asm.next()?;
                        asm.data.push(0xCD);
                    }

                    Some(Token::Symbol { name: SymbolName::ParenOpen, .. }) => {
                        asm.next()?;
                        asm.expect_register(RegisterName::HL)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                        asm.data.push(0xCE);
                    }

                    Some(_) => {
                        asm.data.push(0xFE);
                        asm.expect_immediate()?;
                    }
                }
            }

            OperationName::Ret => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err()?,
                    Some(Token::Flag {
                        name: FlagName::NZ,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xC0);
                    }
                    Some(Token::Flag {
                        name: FlagName::Z,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xC8);
                    }
                    Some(Token::Flag {
                        name: FlagName::NC,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xD0);
                    }
                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    } | Token::Flag { name: FlagName::C, .. }) => {
                        asm.next()?;
                        asm.data.push(0xD8);
                    }
                    Some(_) => {
                        asm.data.push(0xC9);
                    }
                }
            }

            OperationName::Reti => {
                asm.next()?;
                asm.data.push(0xD9);
            }

            OperationName::Jp => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Flag { name: FlagName::NZ, .. }) => {
                        asm.next()?;
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.data.push(0xC2);
                        asm.expect_wide_immediate()?;
                    }

                    Some(Token::Flag { name: FlagName::NC, .. }) => {
                        asm.next()?;
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.data.push(0xD2);
                        asm.expect_wide_immediate()?;
                    }
                    
                    Some(Token::Flag { name: FlagName::Z, .. }) => {
                        asm.next()?;
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.data.push(0xCA);
                        asm.expect_wide_immediate()?;
                    }
                    
                    Some(Token::Register { name: RegisterName::C, ..} | Token::Flag { name: FlagName::C, .. }) => {
                        asm.next()?;
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.data.push(0xDA);
                        asm.expect_wide_immediate()?;
                    }

                    Some(Token::Register { name: RegisterName::HL, .. }) => {
                        asm.next()?;
                        asm.data.push(0xE9);
                    }

                    Some(_) => {
                        asm.data.push(0xC3);
                        asm.expect_wide_immediate()?;
                    }
                }
            }

            OperationName::Call => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Flag { name: FlagName::NZ, .. }) => {
                        asm.next()?;
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.data.push(0xC4);
                        asm.expect_wide_immediate()?;
                    }

                    Some(Token::Flag { name: FlagName::NC, .. }) => {
                        asm.next()?;
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.data.push(0xD4);
                        asm.expect_wide_immediate()?;
                    }
                    
                    Some(Token::Flag { name: FlagName::Z, .. }) => {
                        asm.next()?;
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.data.push(0xCC);
                        asm.expect_wide_immediate()?;
                    }
                    
                    Some(Token::Register { name: RegisterName::C, ..} | Token::Flag { name: FlagName::C, .. }) => {
                        asm.next()?;
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.data.push(0xDC);
                        asm.expect_wide_immediate()?;
                    }

                    Some(_) => {
                        asm.data.push(0xCD);
                        asm.expect_wide_immediate()?;
                    }
                }
            }

            OperationName::Rst => {
                asm.next()?;
                match asm.const_expr()? {
                    (loc, None) => return asm_err!(loc,"The expression following an \"rst\" instruction must be immediately solvable"),
                    (loc, Some(value)) => {
                        let byte = match value {
                            0x00 => 0xC7,
                            0x08 => 0xCF,
                            0x10 => 0xD7,
                            0x18 => 0xDF,
                            0x20 => 0xE7,
                            0x28 => 0xEF,
                            0x30 => 0xF7,
                            0x38 => 0xFF,
                            _ => return asm_err!(loc,"The \"rst\" value ({value}) is not valid"),
                        };
                        asm.data.push(byte);
                    }
                }
            }

            OperationName::Di => {
                asm.next()?;
                asm.data.push(0xF3);
            }

            OperationName::Ei => {
                asm.next()?;
                asm.data.push(0xFB);
            }

            OperationName::Push => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register { name: RegisterName::BC, .. }) => {
                        asm.data.push(0xC5);
                    }
                    
                    Some(Token::Register { name: RegisterName::DE, .. }) => {
                        asm.data.push(0xD5);
                    }
                    
                    Some(Token::Register { name: RegisterName::HL, .. }) => {
                        asm.data.push(0xE5);
                    }
                    
                    Some(Token::Register { name: RegisterName::AF, .. }) => {
                        asm.data.push(0xF5);
                    }

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected register \"bc\", \"de\", \"hl\", or \"af\"",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                }
            }

            OperationName::Pop => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register { name: RegisterName::BC, .. }) => {
                        asm.data.push(0xC1);
                    }
                    
                    Some(Token::Register { name: RegisterName::DE, .. }) => {
                        asm.data.push(0xD1);
                    }
                    
                    Some(Token::Register { name: RegisterName::HL, .. }) => {
                        asm.data.push(0xE1);
                    }
                    
                    Some(Token::Register { name: RegisterName::AF, .. }) => {
                        asm.data.push(0xF1);
                    }

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected register \"bc\", \"de\", \"hl\", or \"af\"",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                }
            }

            OperationName::Rlc => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x07);
                    }

                    Some(Token::Register {
                        name: RegisterName::B,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x00);
                    }

                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x01);
                    }

                    Some(Token::Register {
                        name: RegisterName::D,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x02);
                    }

                    Some(Token::Register {
                        name: RegisterName::E,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x03);
                    }

                    Some(Token::Register {
                        name: RegisterName::H,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x04);
                    }

                    Some(Token::Register {
                        name: RegisterName::L,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x05);
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.expect_register(RegisterName::HL)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                        asm.data.push(0xCB);
                        asm.data.push(0x06);
                    }

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected register",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                }
            }

            OperationName::Rrc => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x0F);
                    }

                    Some(Token::Register {
                        name: RegisterName::B,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x08);
                    }

                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x09);
                    }

                    Some(Token::Register {
                        name: RegisterName::D,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x0A);
                    }

                    Some(Token::Register {
                        name: RegisterName::E,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x0B);
                    }

                    Some(Token::Register {
                        name: RegisterName::H,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x0C);
                    }

                    Some(Token::Register {
                        name: RegisterName::L,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x0D);
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.expect_register(RegisterName::HL)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                        asm.data.push(0xCB);
                        asm.data.push(0x0E);
                    }

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected register",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                }
            }

            OperationName::Rl => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x17);
                    }

                    Some(Token::Register {
                        name: RegisterName::B,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x10);
                    }

                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x11);
                    }

                    Some(Token::Register {
                        name: RegisterName::D,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x12);
                    }

                    Some(Token::Register {
                        name: RegisterName::E,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x13);
                    }

                    Some(Token::Register {
                        name: RegisterName::H,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x14);
                    }

                    Some(Token::Register {
                        name: RegisterName::L,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x15);
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.expect_register(RegisterName::HL)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                        asm.data.push(0xCB);
                        asm.data.push(0x16);
                    }

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected register",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                }
            }

            OperationName::Rr => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x1F);
                    }

                    Some(Token::Register {
                        name: RegisterName::B,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x18);
                    }

                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x19);
                    }

                    Some(Token::Register {
                        name: RegisterName::D,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x1A);
                    }

                    Some(Token::Register {
                        name: RegisterName::E,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x1B);
                    }

                    Some(Token::Register {
                        name: RegisterName::H,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x1C);
                    }

                    Some(Token::Register {
                        name: RegisterName::L,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x1D);
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.expect_register(RegisterName::HL)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                        asm.data.push(0xCB);
                        asm.data.push(0x1E);
                    }

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected register",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                }
            }

            OperationName::Sla => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x27);
                    }

                    Some(Token::Register {
                        name: RegisterName::B,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x20);
                    }

                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x21);
                    }

                    Some(Token::Register {
                        name: RegisterName::D,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x22);
                    }

                    Some(Token::Register {
                        name: RegisterName::E,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x23);
                    }

                    Some(Token::Register {
                        name: RegisterName::H,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x24);
                    }

                    Some(Token::Register {
                        name: RegisterName::L,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x25);
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.expect_register(RegisterName::HL)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                        asm.data.push(0xCB);
                        asm.data.push(0x26);
                    }

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected register",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                }
            }

            OperationName::Sra => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x2F);
                    }

                    Some(Token::Register {
                        name: RegisterName::B,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x28);
                    }

                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x29);
                    }

                    Some(Token::Register {
                        name: RegisterName::D,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x2A);
                    }

                    Some(Token::Register {
                        name: RegisterName::E,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x2B);
                    }

                    Some(Token::Register {
                        name: RegisterName::H,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x2C);
                    }

                    Some(Token::Register {
                        name: RegisterName::L,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x2D);
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.expect_register(RegisterName::HL)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                        asm.data.push(0xCB);
                        asm.data.push(0x2E);
                    }

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected register",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                }
            }

            OperationName::Swap => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x37);
                    }

                    Some(Token::Register {
                        name: RegisterName::B,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x30);
                    }

                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x31);
                    }

                    Some(Token::Register {
                        name: RegisterName::D,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x32);
                    }

                    Some(Token::Register {
                        name: RegisterName::E,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x33);
                    }

                    Some(Token::Register {
                        name: RegisterName::H,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x34);
                    }

                    Some(Token::Register {
                        name: RegisterName::L,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x35);
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.expect_register(RegisterName::HL)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                        asm.data.push(0xCB);
                        asm.data.push(0x36);
                    }

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected register",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                }
            }

            OperationName::Srl => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x3F);
                    }

                    Some(Token::Register {
                        name: RegisterName::B,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x38);
                    }

                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x39);
                    }

                    Some(Token::Register {
                        name: RegisterName::D,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x3A);
                    }

                    Some(Token::Register {
                        name: RegisterName::E,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x3B);
                    }

                    Some(Token::Register {
                        name: RegisterName::H,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x3C);
                    }

                    Some(Token::Register {
                        name: RegisterName::L,
                        ..
                    }) => {
                        asm.data.push(0xCB);
                        asm.data.push(0x3D);
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.expect_register(RegisterName::HL)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                        asm.data.push(0xCB);
                        asm.data.push(0x3E);
                    }

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected register",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                }
            }

            OperationName::Bit => {
                asm.next()?;
                match asm.const_expr()? {
                    (loc, None) => return asm_err!(loc, "Bit index must be immediately solvable"),
                    (loc, Some(value)) => {
                        if !(0..=7).contains(&value) {
                            return asm_err!(loc, "Bit index ({value}) must be between 0 and 7");
                        }
                        asm.expect_symbol(SymbolName::Comma)?;
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            
                            Some(Token::Register {
                                name: RegisterName::A,
                                ..
                            }) => {
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0x47,
                                    1 => 0x4F,
                                    2 => 0x57,
                                    3 => 0x5F,
                                    4 => 0x67,
                                    5 => 0x6F,
                                    6 => 0x77,
                                    7 => 0x7F,
                                    _ => unreachable!(),
                                });
                            }

                            Some(Token::Register {
                                name: RegisterName::B,
                                ..
                            }) => {
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0x40,
                                    1 => 0x48,
                                    2 => 0x50,
                                    3 => 0x58,
                                    4 => 0x60,
                                    5 => 0x68,
                                    6 => 0x70,
                                    7 => 0x78,
                                    _ => unreachable!(),
                                });
                            }

                            Some(Token::Register {
                                name: RegisterName::C,
                                ..
                            }) => {
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0x41,
                                    1 => 0x49,
                                    2 => 0x51,
                                    3 => 0x59,
                                    4 => 0x61,
                                    5 => 0x69,
                                    6 => 0x71,
                                    7 => 0x79,
                                    _ => unreachable!(),
                                });
                            }

                            Some(Token::Register {
                                name: RegisterName::D,
                                ..
                            }) => {
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0x42,
                                    1 => 0x4A,
                                    2 => 0x52,
                                    3 => 0x5A,
                                    4 => 0x62,
                                    5 => 0x6A,
                                    6 => 0x72,
                                    7 => 0x7A,
                                    _ => unreachable!(),
                                });
                            }

                            Some(Token::Register {
                                name: RegisterName::E,
                                ..
                            }) => {
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0x43,
                                    1 => 0x4B,
                                    2 => 0x53,
                                    3 => 0x5B,
                                    4 => 0x63,
                                    5 => 0x6B,
                                    6 => 0x73,
                                    7 => 0x7B,
                                    _ => unreachable!(),
                                });
                            }

                            Some(Token::Register {
                                name: RegisterName::H,
                                ..
                            }) => {
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0x44,
                                    1 => 0x4C,
                                    2 => 0x54,
                                    3 => 0x5C,
                                    4 => 0x64,
                                    5 => 0x6C,
                                    6 => 0x74,
                                    7 => 0x7C,
                                    _ => unreachable!(),
                                });
                            }

                            Some(Token::Register {
                                name: RegisterName::L,
                                ..
                            }) => {
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0x45,
                                    1 => 0x4D,
                                    2 => 0x55,
                                    3 => 0x5D,
                                    4 => 0x65,
                                    5 => 0x6D,
                                    6 => 0x75,
                                    7 => 0x7D,
                                    _ => unreachable!(),
                                });
                            }

                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            }) => {
                                asm.expect_register(RegisterName::HL)?;
                                asm.expect_symbol(SymbolName::ParenClose)?;
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0x46,
                                    1 => 0x4E,
                                    2 => 0x56,
                                    3 => 0x5E,
                                    4 => 0x66,
                                    5 => 0x6E,
                                    6 => 0x76,
                                    7 => 0x7E,
                                    _ => unreachable!(),
                                });
                            }

                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected a register",
                                    tok.as_display(&asm.str_interner)
                                )
                            }
                        }
                    }
                }
            }

            OperationName::Res => {
                asm.next()?;
                match asm.const_expr()? {
                    (loc, None) => return asm_err!(loc, "Bit index must be immediately solvable"),
                    (loc, Some(value)) => {
                        if !(0..=7).contains(&value) {
                            return asm_err!(loc, "Bit index ({value}) must be between 0 and 7");
                        }
                        asm.expect_symbol(SymbolName::Comma)?;
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            
                            Some(Token::Register {
                                name: RegisterName::A,
                                ..
                            }) => {
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0x87,
                                    1 => 0x8F,
                                    2 => 0x97,
                                    3 => 0x9F,
                                    4 => 0xA7,
                                    5 => 0xAF,
                                    6 => 0xB7,
                                    7 => 0xBF,
                                    _ => unreachable!(),
                                });  
                            }  
  
                            Some(Token::Register {
                                name: RegisterName::B,
                                ..  
                            }) => {  
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0x80,
                                    1 => 0x88,
                                    2 => 0x90,
                                    3 => 0x98,
                                    4 => 0xA0,
                                    5 => 0xA8,
                                    6 => 0xB0,
                                    7 => 0xB8,
                                    _ => unreachable!(),
                                });  
                            }  
  
                            Some(Token::Register {
                                name: RegisterName::C,
                                ..  
                            }) => {  
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0x81,
                                    1 => 0x89,
                                    2 => 0x91,
                                    3 => 0x99,
                                    4 => 0xA1,
                                    5 => 0xA9,
                                    6 => 0xB1,
                                    7 => 0xB9,
                                    _ => unreachable!(),
                                });  
                            }  
  
                            Some(Token::Register {
                                name: RegisterName::D,
                                ..  
                            }) => {  
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0x82,
                                    1 => 0x8A,
                                    2 => 0x92,
                                    3 => 0x9A,
                                    4 => 0xA2,
                                    5 => 0xAA,
                                    6 => 0xB2,
                                    7 => 0xBA,
                                    _ => unreachable!(),
                                });  
                            }  
  
                            Some(Token::Register {
                                name: RegisterName::E,
                                ..  
                            }) => {  
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0x83,
                                    1 => 0x8B,
                                    2 => 0x93,
                                    3 => 0x9B,
                                    4 => 0xA3,
                                    5 => 0xAB,
                                    6 => 0xB3,
                                    7 => 0xBB,
                                    _ => unreachable!(),
                                });  
                            }  
  
                            Some(Token::Register {
                                name: RegisterName::H,
                                ..  
                            }) => {  
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0x84,
                                    1 => 0x8C,
                                    2 => 0x94,
                                    3 => 0x9C,
                                    4 => 0xA4,
                                    5 => 0xAC,
                                    6 => 0xB4,
                                    7 => 0xBC,
                                    _ => unreachable!(),
                                });  
                            }  
  
                            Some(Token::Register {
                                name: RegisterName::L,
                                ..  
                            }) => {  
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0x85,
                                    1 => 0x8D,
                                    2 => 0x95,
                                    3 => 0x9D,
                                    4 => 0xA5,
                                    5 => 0xAD,
                                    6 => 0xB5,
                                    7 => 0xBD,
                                    _ => unreachable!(),
                                });  
                            }  
  
                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..  
                            }) => {  
                                asm.expect_register(RegisterName::HL)?;
                                asm.expect_symbol(SymbolName::ParenClose)?;
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0x86,
                                    1 => 0x8E,
                                    2 => 0x96,
                                    3 => 0x9E,
                                    4 => 0xA6,
                                    5 => 0xAE,
                                    6 => 0xB6,
                                    7 => 0xBE,
                                    _ => unreachable!(),
                                });  
                            }  
  
                            Some(tok) => {  
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected a register",
                                    tok.as_display(&asm.str_interner)
                                )  
                            }  
                        }
                    }
                }
            }

            OperationName::Set => {
                asm.next()?;
                match asm.const_expr()? {
                    (loc, None) => return asm_err!(loc, "Bit index must be immediately solvable"),
                    (loc, Some(value)) => {
                        if !(0..=7).contains(&value) {
                            return asm_err!(loc, "Bit index ({value}) must be between 0 and 7");
                        }
                        asm.expect_symbol(SymbolName::Comma)?;
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            
                            Some(Token::Register {
                                name: RegisterName::A,
                                ..
                            }) => {
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0xC7,
                                    1 => 0xCF,
                                    2 => 0xD7,
                                    3 => 0xDF,
                                    4 => 0xE7,
                                    5 => 0xEF,
                                    6 => 0xF7,
                                    7 => 0xFF,
                                    _ => unreachable!(),
                                });    
                            }    
    
                            Some(Token::Register {
                                name: RegisterName::B,
                                ..    
                            }) => {    
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0xC0,
                                    1 => 0xC8,
                                    2 => 0xD0,
                                    3 => 0xD8,
                                    4 => 0xE0,
                                    5 => 0xE8,
                                    6 => 0xF0,
                                    7 => 0xF8,
                                    _ => unreachable!(),
                                });    
                            }    
    
                            Some(Token::Register {
                                name: RegisterName::C,
                                ..    
                            }) => {    
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0xC1,
                                    1 => 0xC9,
                                    2 => 0xD1,
                                    3 => 0xD9,
                                    4 => 0xE1,
                                    5 => 0xE9,
                                    6 => 0xF1,
                                    7 => 0xF9,
                                    _ => unreachable!(),
                                });    
                            }    
    
                            Some(Token::Register {
                                name: RegisterName::D,
                                ..    
                            }) => {    
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0xC2,
                                    1 => 0xCA,
                                    2 => 0xD2,
                                    3 => 0xDA,
                                    4 => 0xE2,
                                    5 => 0xEA,
                                    6 => 0xF2,
                                    7 => 0xFA,
                                    _ => unreachable!(),
                                });    
                            }    
    
                            Some(Token::Register {
                                name: RegisterName::E,
                                ..    
                            }) => {    
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0xC3,
                                    1 => 0xCB,
                                    2 => 0xD3,
                                    3 => 0xDB,
                                    4 => 0xE3,
                                    5 => 0xEB,
                                    6 => 0xF3,
                                    7 => 0xFB,
                                    _ => unreachable!(),
                                });    
                            }    
    
                            Some(Token::Register {
                                name: RegisterName::H,
                                ..    
                            }) => {    
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0xC4,
                                    1 => 0xCC,
                                    2 => 0xD4,
                                    3 => 0xDC,
                                    4 => 0xE4,
                                    5 => 0xEC,
                                    6 => 0xF4,
                                    7 => 0xFC,
                                    _ => unreachable!(),
                                });    
                            }    
    
                            Some(Token::Register {
                                name: RegisterName::L,
                                ..    
                            }) => {    
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0xC5,
                                    1 => 0xCD,
                                    2 => 0xD5,
                                    3 => 0xDD,
                                    4 => 0xE5,
                                    5 => 0xED,
                                    6 => 0xF5,
                                    7 => 0xFD,
                                    _ => unreachable!(),
                                });    
                            }    
    
                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..    
                            }) => {    
                                asm.expect_register(RegisterName::HL)?;
                                asm.expect_symbol(SymbolName::ParenClose)?;
                                asm.data.push(0xCB);
                                asm.data.push(match value {
                                    0 => 0xC6,
                                    1 => 0xCE,
                                    2 => 0xD6,
                                    3 => 0xDE,
                                    4 => 0xE6,
                                    5 => 0xEE,
                                    6 => 0xF6,
                                    7 => 0xFE,
                                    _ => unreachable!(),
                                });    
                            }    
    
                            Some(tok) => {    
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected a register",
                                    tok.as_display(&asm.str_interner)
                                )    
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

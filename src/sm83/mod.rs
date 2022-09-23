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
    Rrca,
    Stop,
    Rla,
    Jr,
    Rra,
    Daa,
    Cpl,
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
            "RRCA" | "rrca" => Some(Self::Rrca),
            "STOP" | "stop" => Some(Self::Stop),
            "RLA" | "rla" => Some(Self::Rla),
            "JR" | "jr" => Some(Self::Jr),
            "RRA" | "rra" => Some(Self::Rra),
            "DAA" | "daa" => Some(Self::Daa),
            "CPL" | "cpl" => Some(Self::Cpl),
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
                Self::Rrca => "rrca",
                Self::Stop => "stop",
                Self::Rla => "rla",
                Self::Jr => "jr",
                Self::Rra => "rra",
                Self::Daa => "daa",
                Self::Cpl => "cpl",
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
                        todo!()
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

                    Some(tok) => {
                        return asm_err!(
                        tok.loc(),
                        "Unexpected {}, expected a valid destination register or indirect address",
                        tok.as_display(&asm.str_interner)
                    )
                    }
                }
            }

            _ => unimplemented!("{name}"),
        }
        Ok(())
    }
}

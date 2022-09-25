use std::{
    fmt::{self, Display, Formatter},
    io::Read,
};

use crate::{
    assembler::{ArchAssembler, Assembler, AssemblerError},
    fileman::FileSystem,
    lexer::{self, ArchTokens, FlagName, SourceLoc, SymbolName, Token},
    linker::Link,
};

#[cfg(test)]
mod tests;

mod namelist;

pub use namelist::*;

macro_rules! asm_err {
    ($loc:expr, $($arg:tt)*) => {
        Err(($loc, AssemblerError(format!($($arg)*))))
    };
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum OperationName {
    Adc,
    And,
    Asl,
    Bcc,
    Bcs,
    Beq,
    Bit,
    Bmi,
    Bne,
    Bpl,
    Brk,
    Bvc,
    Bvs,
    Clc,
    Cld,
    Cli,
    Clv,
    Cmp,
    Cpx,
    Cpy,
    Dec,
    Dex,
    Dey,
    Eor,
    Inc,
    Inx,
    Iny,
    Jmp,
    Jsr,
    Lda,
    Ldx,
    Ldy,
    Lsr,
    Nop,
    Ora,
    Pha,
    Php,
    Pla,
    Plp,
    Rol,
    Ror,
    Rti,
    Rts,
    Sbc,
    Sec,
    Sed,
    Sei,
    Sta,
    Stx,
    Sty,
    Tax,
    Tay,
    Tsx,
    Txa,
    Txs,
    Tya,
}

impl lexer::OperationName for OperationName {
    fn parse<S: AsRef<str>>(s: S) -> Option<Self> {
        match s.as_ref() {
            "adc" | "ADC" => Some(Self::Adc),
            "and" | "AND" => Some(Self::And),
            "asl" | "ASL" => Some(Self::Asl),
            "bcc" | "BCC" => Some(Self::Bcc),
            "bcs" | "BCS" => Some(Self::Bcs),
            "beq" | "BEQ" => Some(Self::Beq),
            "bit" | "BIT" => Some(Self::Bit),
            "bmi" | "BMI" => Some(Self::Bmi),
            "bne" | "BNE" => Some(Self::Bne),
            "bpl" | "BPL" => Some(Self::Bpl),
            "brk" | "BRK" => Some(Self::Brk),
            "bvc" | "BVC" => Some(Self::Bvc),
            "bvs" | "BVS" => Some(Self::Bvs),
            "clc" | "CLC" => Some(Self::Clc),
            "cld" | "CLD" => Some(Self::Cld),
            "cli" | "CLI" => Some(Self::Cli),
            "clv" | "CLV" => Some(Self::Clv),
            "cmp" | "CMP" => Some(Self::Cmp),
            "cpx" | "CPX" => Some(Self::Cpx),
            "cpy" | "CPY" => Some(Self::Cpy),
            "dec" | "DEC" => Some(Self::Dec),
            "dex" | "DEX" => Some(Self::Dex),
            "dey" | "DEY" => Some(Self::Dey),
            "eor" | "EOR" => Some(Self::Eor),
            "inc" | "INC" => Some(Self::Inc),
            "inx" | "INX" => Some(Self::Inx),
            "iny" | "INY" => Some(Self::Iny),
            "jmp" | "JMP" => Some(Self::Jmp),
            "jsr" | "JSR" => Some(Self::Jsr),
            "lda" | "LDA" => Some(Self::Lda),
            "ldx" | "LDX" => Some(Self::Ldx),
            "ldy" | "LDY" => Some(Self::Ldy),
            "lsr" | "LSR" => Some(Self::Lsr),
            "nop" | "NOP" => Some(Self::Nop),
            "ora" | "ORA" => Some(Self::Ora),
            "pha" | "PHA" => Some(Self::Pha),
            "php" | "PHP" => Some(Self::Php),
            "pla" | "PLA" => Some(Self::Pla),
            "plp" | "PLP" => Some(Self::Plp),
            "rol" | "ROL" => Some(Self::Rol),
            "ror" | "ROR" => Some(Self::Ror),
            "rti" | "RTI" => Some(Self::Rti),
            "rts" | "RTS" => Some(Self::Rts),
            "sbc" | "SBC" => Some(Self::Sbc),
            "sec" | "SEC" => Some(Self::Sec),
            "sed" | "SED" => Some(Self::Sed),
            "sei" | "SEI" => Some(Self::Sei),
            "sta" | "STA" => Some(Self::Sta),
            "stx" | "STX" => Some(Self::Stx),
            "sty" | "STY" => Some(Self::Sty),
            "tax" | "TAA" => Some(Self::Tax),
            "tay" | "TAY" => Some(Self::Tay),
            "tsx" | "TSX" => Some(Self::Tsx),
            "txa" | "TXA" => Some(Self::Txa),
            "txs" | "TXS" => Some(Self::Txs),
            "tya" | "TYA" => Some(Self::Tya),
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
                Self::Adc => "adc",
                Self::And => "and",
                Self::Asl => "asl",
                Self::Bcc => "bcc",
                Self::Bcs => "bcs",
                Self::Beq => "beq",
                Self::Bit => "bit",
                Self::Bmi => "bmi",
                Self::Bne => "bne",
                Self::Bpl => "bpl",
                Self::Brk => "brk",
                Self::Bvc => "bvc",
                Self::Bvs => "bvs",
                Self::Clc => "clc",
                Self::Cld => "cld",
                Self::Cli => "cli",
                Self::Clv => "clv",
                Self::Cmp => "cmp",
                Self::Cpx => "cpx",
                Self::Cpy => "cpy",
                Self::Dec => "dec",
                Self::Dex => "dex",
                Self::Dey => "dey",
                Self::Eor => "eor",
                Self::Inc => "inc",
                Self::Inx => "inx",
                Self::Iny => "iny",
                Self::Jmp => "jmp",
                Self::Jsr => "jsr",
                Self::Lda => "lda",
                Self::Ldx => "ldx",
                Self::Ldy => "ldy",
                Self::Lsr => "lsr",
                Self::Nop => "nop",
                Self::Ora => "ora",
                Self::Pha => "pha",
                Self::Php => "php",
                Self::Pla => "pla",
                Self::Plp => "plp",
                Self::Rol => "rol",
                Self::Ror => "ror",
                Self::Rti => "rti",
                Self::Rts => "rts",
                Self::Sbc => "sbc",
                Self::Sec => "sec",
                Self::Sed => "sed",
                Self::Sei => "sei",
                Self::Sta => "sta",
                Self::Stx => "stx",
                Self::Sty => "sty",
                Self::Tax => "tax",
                Self::Tay => "tay",
                Self::Tsx => "tsx",
                Self::Txa => "txa",
                Self::Txs => "txs",
                Self::Tya => "tya",
            }
        )
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum RegisterName {
    A,
    X,
    Y,
}

impl lexer::RegisterName for RegisterName {
    fn parse<S: AsRef<str>>(s: S) -> Option<Self> {
        match s.as_ref() {
            "a" | "A" => Some(Self::A),
            "x" | "X" => Some(Self::X),
            "y" | "Y" => Some(Self::Y),
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
                Self::X => "x",
                Self::Y => "y",
            }
        )
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FakeFlagName;

impl FlagName for FakeFlagName {
    fn parse<S: AsRef<str>>(_: S) -> Option<Self> {
        None
    }
}

impl Display for FakeFlagName {
    fn fmt(&self, _: &mut Formatter<'_>) -> fmt::Result {
        Err(fmt::Error)
    }
}

pub struct Mos6502;

#[derive(Copy, Clone)]
pub struct Mos6502Tokens;

impl ArchTokens for Mos6502Tokens {
    type RegisterName = RegisterName;
    type FlagName = FakeFlagName;
    type OperationName = OperationName;
}

impl<S, R> ArchAssembler<S, R, Mos6502Tokens> for Mos6502
where
    S: FileSystem<Reader = R>,
    R: Read,
{
    fn parse(
        asm: &mut Assembler<S, R, Mos6502Tokens, Self>,
        name: OperationName,
    ) -> Result<(), (SourceLoc, AssemblerError)> {
        match name {
            OperationName::Adc => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Symbol {
                        name: SymbolName::Hash,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0x69);
                        asm.expect_immediate()?;
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.next()?;
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u8::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a byte"
                                );
                            }
                            value as u8
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::byte(loc, asm.data.len() + 1, expr));
                            0
                        };
                        match asm.next()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Symbol {
                                name: SymbolName::Comma,
                                ..
                            }) => {
                                asm.data.push(0x61);
                                asm.expect_register(RegisterName::X)?;
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(Token::Symbol {
                                name: SymbolName::ParenClose,
                                ..
                            }) => {
                                asm.data.push(0x71);
                                asm.expect_symbol(SymbolName::Comma)?;
                                asm.expect_register(RegisterName::Y)?;
                            }

                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected \",\" or \")\"",
                                    tok.as_display(&asm.str_interner)
                                );
                            }
                        }
                        asm.data.push(value);
                    }

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    asm.next()?;
                                    asm.expect_register(RegisterName::X)?;
                                    asm.data.push(0x75);
                                } else {
                                    asm.data.push(0x65);
                                }
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                            asm.next()?;
                            match asm.next()? {
                                None => return asm.end_of_input_err(),

                                Some(Token::Register {
                                    name: RegisterName::X,
                                    ..
                                }) => {
                                    asm.data.push(0x7D);
                                }

                                Some(Token::Register {
                                    name: RegisterName::Y,
                                    ..
                                }) => {
                                    asm.data.push(0x79);
                                }

                                Some(tok) => {
                                    return asm_err!(
                                        tok.loc(),
                                        "Unexpected {}, expected register \"x\" or \"y\"",
                                        tok.as_display(&asm.str_interner)
                                    );
                                }
                            }
                        } else {
                            asm.data.push(0x6D);
                        }
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::And => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Symbol {
                        name: SymbolName::Hash,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0x29);
                        asm.expect_immediate()?;
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.next()?;
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u8::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a byte"
                                );
                            }
                            value as u8
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::byte(loc, asm.data.len() + 1, expr));
                            0
                        };
                        match asm.next()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Symbol {
                                name: SymbolName::Comma,
                                ..
                            }) => {
                                asm.data.push(0x21);
                                asm.expect_register(RegisterName::X)?;
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(Token::Symbol {
                                name: SymbolName::ParenClose,
                                ..
                            }) => {
                                asm.data.push(0x31);
                                asm.expect_symbol(SymbolName::Comma)?;
                                asm.expect_register(RegisterName::Y)?;
                            }

                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected \",\" or \")\"",
                                    tok.as_display(&asm.str_interner)
                                );
                            }
                        }
                        asm.data.push(value);
                    }

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    asm.next()?;
                                    asm.expect_register(RegisterName::X)?;
                                    asm.data.push(0x35);
                                } else {
                                    asm.data.push(0x25);
                                }
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                            asm.next()?;
                            match asm.next()? {
                                None => return asm.end_of_input_err(),

                                Some(Token::Register {
                                    name: RegisterName::X,
                                    ..
                                }) => {
                                    asm.data.push(0x3D);
                                }

                                Some(Token::Register {
                                    name: RegisterName::Y,
                                    ..
                                }) => {
                                    asm.data.push(0x39);
                                }

                                Some(tok) => {
                                    return asm_err!(
                                        tok.loc(),
                                        "Unexpected {}, expected register \"x\" or \"y\"",
                                        tok.as_display(&asm.str_interner)
                                    );
                                }
                            }
                        } else {
                            asm.data.push(0x2D);
                        }
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Asl => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0x0A);
                    }

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    asm.next()?;
                                    asm.expect_register(RegisterName::X)?;
                                    asm.data.push(0x16);
                                } else {
                                    asm.data.push(0x06);
                                }
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                            asm.next()?;
                            asm.expect_register(RegisterName::X)?;
                            asm.data.push(0x1E);
                        } else {
                            asm.data.push(0x0E);
                        }
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Bcc => {
                asm.next()?;
                asm.data.push(0x90);
                asm.expect_branch_immediate()?;
            }

            OperationName::Bcs => {
                asm.next()?;
                asm.data.push(0xB0);
                asm.expect_branch_immediate()?;
            }

            OperationName::Beq => {
                asm.next()?;
                asm.data.push(0xF0);
                asm.expect_branch_immediate()?;
            }

            OperationName::Bit => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                asm.data.push(0x24);
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        asm.data.push(0x2C);
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Bmi => {
                asm.next()?;
                asm.data.push(0x30);
                asm.expect_branch_immediate()?;
            }

            OperationName::Bne => {
                asm.next()?;
                asm.data.push(0xD0);
                asm.expect_branch_immediate()?;
            }

            OperationName::Bpl => {
                asm.next()?;
                asm.data.push(0x10);
                asm.expect_branch_immediate()?;
            }

            OperationName::Brk => {
                asm.next()?;
                asm.data.push(0x00);
            }

            OperationName::Bvc => {
                asm.next()?;
                asm.data.push(0x50);
                asm.expect_branch_immediate()?;
            }

            OperationName::Bvs => {
                asm.next()?;
                asm.data.push(0x70);
                asm.expect_branch_immediate()?;
            }

            OperationName::Clc => {
                asm.next()?;
                asm.data.push(0x18);
            }

            OperationName::Cld => {
                asm.next()?;
                asm.data.push(0xD8);
            }

            OperationName::Cli => {
                asm.next()?;
                asm.data.push(0x58);
            }

            OperationName::Clv => {
                asm.next()?;
                asm.data.push(0xB8);
            }

            OperationName::Cmp => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Symbol {
                        name: SymbolName::Hash,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xC9);
                        asm.expect_immediate()?;
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.next()?;
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u8::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a byte"
                                );
                            }
                            value as u8
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::byte(loc, asm.data.len() + 1, expr));
                            0
                        };
                        match asm.next()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Symbol {
                                name: SymbolName::Comma,
                                ..
                            }) => {
                                asm.data.push(0xC1);
                                asm.expect_register(RegisterName::X)?;
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(Token::Symbol {
                                name: SymbolName::ParenClose,
                                ..
                            }) => {
                                asm.data.push(0xD1);
                                asm.expect_symbol(SymbolName::Comma)?;
                                asm.expect_register(RegisterName::Y)?;
                            }

                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected \",\" or \")\"",
                                    tok.as_display(&asm.str_interner)
                                );
                            }
                        }
                        asm.data.push(value);
                    }

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    asm.next()?;
                                    asm.expect_register(RegisterName::X)?;
                                    asm.data.push(0xD5);
                                } else {
                                    asm.data.push(0xC5);
                                }
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                            asm.next()?;
                            match asm.next()? {
                                None => return asm.end_of_input_err(),

                                Some(Token::Register {
                                    name: RegisterName::X,
                                    ..
                                }) => {
                                    asm.data.push(0xDD);
                                }

                                Some(Token::Register {
                                    name: RegisterName::Y,
                                    ..
                                }) => {
                                    asm.data.push(0xD9);
                                }

                                Some(tok) => {
                                    return asm_err!(
                                        tok.loc(),
                                        "Unexpected {}, expected register \"x\" or \"y\"",
                                        tok.as_display(&asm.str_interner)
                                    );
                                }
                            }
                        } else {
                            asm.data.push(0xCD);
                        }
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Cpx => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Symbol {
                        name: SymbolName::Hash,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xE0);
                        asm.expect_immediate()?;
                    }

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                asm.data.push(0xE4);
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        asm.data.push(0xEC);
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Cpy => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Symbol {
                        name: SymbolName::Hash,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xC0);
                        asm.expect_immediate()?;
                    }

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                asm.data.push(0xC4);
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        asm.data.push(0xCC);
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Dec => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    asm.next()?;
                                    asm.expect_register(RegisterName::X)?;
                                    asm.data.push(0xD6);
                                } else {
                                    asm.data.push(0xC6);
                                }
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                            asm.next()?;
                            asm.expect_register(RegisterName::X)?;
                            asm.data.push(0xDE);
                        } else {
                            asm.data.push(0xCE);
                        }
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Dex => {
                asm.next()?;
                asm.data.push(0xCA);
            }

            OperationName::Dey => {
                asm.next()?;
                asm.data.push(0x88);
            }

            OperationName::Eor => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Symbol {
                        name: SymbolName::Hash,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0x49);
                        asm.expect_immediate()?;
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.next()?;
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u8::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a byte"
                                );
                            }
                            value as u8
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::byte(loc, asm.data.len() + 1, expr));
                            0
                        };
                        match asm.next()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Symbol {
                                name: SymbolName::Comma,
                                ..
                            }) => {
                                asm.data.push(0x41);
                                asm.expect_register(RegisterName::X)?;
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(Token::Symbol {
                                name: SymbolName::ParenClose,
                                ..
                            }) => {
                                asm.data.push(0x51);
                                asm.expect_symbol(SymbolName::Comma)?;
                                asm.expect_register(RegisterName::Y)?;
                            }

                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected \",\" or \")\"",
                                    tok.as_display(&asm.str_interner)
                                );
                            }
                        }
                        asm.data.push(value);
                    }

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    asm.next()?;
                                    asm.expect_register(RegisterName::X)?;
                                    asm.data.push(0x55);
                                } else {
                                    asm.data.push(0x45);
                                }
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                            asm.next()?;
                            match asm.next()? {
                                None => return asm.end_of_input_err(),

                                Some(Token::Register {
                                    name: RegisterName::X,
                                    ..
                                }) => {
                                    asm.data.push(0x5D);
                                }

                                Some(Token::Register {
                                    name: RegisterName::Y,
                                    ..
                                }) => {
                                    asm.data.push(0x59);
                                }

                                Some(tok) => {
                                    return asm_err!(
                                        tok.loc(),
                                        "Unexpected {}, expected register \"x\" or \"y\"",
                                        tok.as_display(&asm.str_interner)
                                    );
                                }
                            }
                        } else {
                            asm.data.push(0x4D);
                        }
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Inc => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    asm.next()?;
                                    asm.expect_register(RegisterName::X)?;
                                    asm.data.push(0xF6);
                                } else {
                                    asm.data.push(0xE6);
                                }
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                            asm.next()?;
                            asm.expect_register(RegisterName::X)?;
                            asm.data.push(0xFE);
                        } else {
                            asm.data.push(0xEE);
                        }
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Inx => {
                asm.next()?;
                asm.data.push(0xE8);
            }

            OperationName::Iny => {
                asm.next()?;
                asm.data.push(0xC8);
            }

            OperationName::Jmp => {
                asm.next()?;

                let need_paren = if asm.peeked_symbol(SymbolName::ParenOpen)?.is_some() {
                    asm.next()?;
                    asm.data.push(0x6C);
                    true
                } else {
                    asm.data.push(0x4C);
                    false
                };

                let (loc, expr) = asm.expr()?;
                if let Some(value) = expr.evaluate(&asm.symtab) {
                    if (value as u32) > (u16::MAX as u32) {
                        return asm_err!(loc, "Expression result ({value}) will not fit in a word");
                    }
                    asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                } else {
                    asm.links.push(Link::word(loc, asm.data.len(), expr));
                    asm.data.push(0);
                    asm.data.push(0);
                };

                if need_paren {
                    asm.expect_symbol(SymbolName::ParenClose)?;
                }
            }

            OperationName::Jsr => {
                asm.next()?;
                asm.data.push(0x20);
                let (loc, expr) = asm.expr()?;
                if let Some(value) = expr.evaluate(&asm.symtab) {
                    if (value as u32) > (u16::MAX as u32) {
                        return asm_err!(loc, "Expression result ({value}) will not fit in a word");
                    }
                    asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                } else {
                    asm.links.push(Link::word(loc, asm.data.len(), expr));
                    asm.data.push(0);
                    asm.data.push(0);
                };
            }

            OperationName::Lda => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Symbol {
                        name: SymbolName::Hash,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xA9);
                        asm.expect_immediate()?;
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.next()?;
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u8::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a byte"
                                );
                            }
                            value as u8
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::byte(loc, asm.data.len() + 1, expr));
                            0
                        };
                        match asm.next()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Symbol {
                                name: SymbolName::Comma,
                                ..
                            }) => {
                                asm.data.push(0xA1);
                                asm.expect_register(RegisterName::X)?;
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(Token::Symbol {
                                name: SymbolName::ParenClose,
                                ..
                            }) => {
                                asm.data.push(0xB1);
                                asm.expect_symbol(SymbolName::Comma)?;
                                asm.expect_register(RegisterName::Y)?;
                            }

                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected \",\" or \")\"",
                                    tok.as_display(&asm.str_interner)
                                );
                            }
                        }
                        asm.data.push(value);
                    }

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    asm.next()?;
                                    asm.expect_register(RegisterName::X)?;
                                    asm.data.push(0xB5);
                                } else {
                                    asm.data.push(0xA5);
                                }
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                            asm.next()?;
                            match asm.next()? {
                                None => return asm.end_of_input_err(),

                                Some(Token::Register {
                                    name: RegisterName::X,
                                    ..
                                }) => {
                                    asm.data.push(0xBD);
                                }

                                Some(Token::Register {
                                    name: RegisterName::Y,
                                    ..
                                }) => {
                                    asm.data.push(0xB9);
                                }

                                Some(tok) => {
                                    return asm_err!(
                                        tok.loc(),
                                        "Unexpected {}, expected register \"x\" or \"y\"",
                                        tok.as_display(&asm.str_interner)
                                    );
                                }
                            }
                        } else {
                            asm.data.push(0xAD);
                        }
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Ldx => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Symbol {
                        name: SymbolName::Hash,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xA2);
                        asm.expect_immediate()?;
                    }

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    asm.next()?;
                                    asm.expect_register(RegisterName::Y)?;
                                    asm.data.push(0xB6);
                                } else {
                                    asm.data.push(0xA6);
                                }
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                            asm.next()?;
                            asm.expect_register(RegisterName::Y)?;
                            asm.data.push(0xBE);
                        } else {
                            asm.data.push(0xAE);
                        }
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Ldy => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Symbol {
                        name: SymbolName::Hash,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xA0);
                        asm.expect_immediate()?;
                    }

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    asm.next()?;
                                    asm.expect_register(RegisterName::X)?;
                                    asm.data.push(0xB4);
                                } else {
                                    asm.data.push(0xA4);
                                }
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                            asm.next()?;
                            asm.expect_register(RegisterName::X)?;
                            asm.data.push(0xBC);
                        } else {
                            asm.data.push(0xAC);
                        }
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Lsr => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0x4A);
                    }

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    asm.next()?;
                                    asm.expect_register(RegisterName::X)?;
                                    asm.data.push(0x56);
                                } else {
                                    asm.data.push(0x46);
                                }
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                            asm.next()?;
                            asm.expect_register(RegisterName::X)?;
                            asm.data.push(0x5E);
                        } else {
                            asm.data.push(0x4E);
                        }
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Nop => {
                asm.next()?;
                asm.data.push(0xEA);
            }

            OperationName::Ora => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Symbol {
                        name: SymbolName::Hash,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0x09);
                        asm.expect_immediate()?;
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.next()?;
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u8::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a byte"
                                );
                            }
                            value as u8
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::byte(loc, asm.data.len() + 1, expr));
                            0
                        };
                        match asm.next()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Symbol {
                                name: SymbolName::Comma,
                                ..
                            }) => {
                                asm.data.push(0x01);
                                asm.expect_register(RegisterName::X)?;
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(Token::Symbol {
                                name: SymbolName::ParenClose,
                                ..
                            }) => {
                                asm.data.push(0x11);
                                asm.expect_symbol(SymbolName::Comma)?;
                                asm.expect_register(RegisterName::Y)?;
                            }

                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected \",\" or \")\"",
                                    tok.as_display(&asm.str_interner)
                                );
                            }
                        }
                        asm.data.push(value);
                    }

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    asm.next()?;
                                    asm.expect_register(RegisterName::X)?;
                                    asm.data.push(0x15);
                                } else {
                                    asm.data.push(0x05);
                                }
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                            asm.next()?;
                            match asm.next()? {
                                None => return asm.end_of_input_err(),

                                Some(Token::Register {
                                    name: RegisterName::X,
                                    ..
                                }) => {
                                    asm.data.push(0x1D);
                                }

                                Some(Token::Register {
                                    name: RegisterName::Y,
                                    ..
                                }) => {
                                    asm.data.push(0x19);
                                }

                                Some(tok) => {
                                    return asm_err!(
                                        tok.loc(),
                                        "Unexpected {}, expected register \"x\" or \"y\"",
                                        tok.as_display(&asm.str_interner)
                                    );
                                }
                            }
                        } else {
                            asm.data.push(0x0D);
                        }
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Pha => {
                asm.next()?;
                asm.data.push(0x48);
            }

            OperationName::Php => {
                asm.next()?;
                asm.data.push(0x08);
            }

            OperationName::Pla => {
                asm.next()?;
                asm.data.push(0x68);
            }

            OperationName::Plp => {
                asm.next()?;
                asm.data.push(0x28);
            }

            OperationName::Rol => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0x2A);
                    }

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    asm.next()?;
                                    asm.expect_register(RegisterName::X)?;
                                    asm.data.push(0x36);
                                } else {
                                    asm.data.push(0x26);
                                }
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                            asm.next()?;
                            asm.expect_register(RegisterName::X)?;
                            asm.data.push(0x3E);
                        } else {
                            asm.data.push(0x2E);
                        }
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Ror => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0x6A);
                    }

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    asm.next()?;
                                    asm.expect_register(RegisterName::X)?;
                                    asm.data.push(0x76);
                                } else {
                                    asm.data.push(0x66);
                                }
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                            asm.next()?;
                            asm.expect_register(RegisterName::X)?;
                            asm.data.push(0x7E);
                        } else {
                            asm.data.push(0x6E);
                        }
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Rti => {
                asm.next()?;
                asm.data.push(0x40);
            }

            OperationName::Rts => {
                asm.next()?;
                asm.data.push(0x60);
            }

            OperationName::Sbc => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Symbol {
                        name: SymbolName::Hash,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xE9);
                        asm.expect_immediate()?;
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.next()?;
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u8::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a byte"
                                );
                            }
                            value as u8
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::byte(loc, asm.data.len() + 1, expr));
                            0
                        };
                        match asm.next()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Symbol {
                                name: SymbolName::Comma,
                                ..
                            }) => {
                                asm.data.push(0xE1);
                                asm.expect_register(RegisterName::X)?;
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(Token::Symbol {
                                name: SymbolName::ParenClose,
                                ..
                            }) => {
                                asm.data.push(0xF1);
                                asm.expect_symbol(SymbolName::Comma)?;
                                asm.expect_register(RegisterName::Y)?;
                            }

                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected \",\" or \")\"",
                                    tok.as_display(&asm.str_interner)
                                );
                            }
                        }
                        asm.data.push(value);
                    }

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    asm.next()?;
                                    asm.expect_register(RegisterName::X)?;
                                    asm.data.push(0xF5);
                                } else {
                                    asm.data.push(0xE5);
                                }
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                            asm.next()?;
                            match asm.next()? {
                                None => return asm.end_of_input_err(),

                                Some(Token::Register {
                                    name: RegisterName::X,
                                    ..
                                }) => {
                                    asm.data.push(0xFD);
                                }

                                Some(Token::Register {
                                    name: RegisterName::Y,
                                    ..
                                }) => {
                                    asm.data.push(0xF9);
                                }

                                Some(tok) => {
                                    return asm_err!(
                                        tok.loc(),
                                        "Unexpected {}, expected register \"x\" or \"y\"",
                                        tok.as_display(&asm.str_interner)
                                    );
                                }
                            }
                        } else {
                            asm.data.push(0xED);
                        }
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Sec => {
                asm.next()?;
                asm.data.push(0x38);
            }

            OperationName::Sed => {
                asm.next()?;
                asm.data.push(0xF8);
            }

            OperationName::Sei => {
                asm.next()?;
                asm.data.push(0x78);
            }

            OperationName::Sta => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.next()?;
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u8::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a byte"
                                );
                            }
                            value as u8
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::byte(loc, asm.data.len() + 1, expr));
                            0
                        };
                        match asm.next()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Symbol {
                                name: SymbolName::Comma,
                                ..
                            }) => {
                                asm.data.push(0x81);
                                asm.expect_register(RegisterName::X)?;
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(Token::Symbol {
                                name: SymbolName::ParenClose,
                                ..
                            }) => {
                                asm.data.push(0x91);
                                asm.expect_symbol(SymbolName::Comma)?;
                                asm.expect_register(RegisterName::Y)?;
                            }

                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected \",\" or \")\"",
                                    tok.as_display(&asm.str_interner)
                                );
                            }
                        }
                        asm.data.push(value);
                    }

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    asm.next()?;
                                    asm.expect_register(RegisterName::X)?;
                                    asm.data.push(0x95);
                                } else {
                                    asm.data.push(0x85);
                                }
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                            asm.next()?;
                            match asm.next()? {
                                None => return asm.end_of_input_err(),

                                Some(Token::Register {
                                    name: RegisterName::X,
                                    ..
                                }) => {
                                    asm.data.push(0x9D);
                                }

                                Some(Token::Register {
                                    name: RegisterName::Y,
                                    ..
                                }) => {
                                    asm.data.push(0x99);
                                }

                                Some(tok) => {
                                    return asm_err!(
                                        tok.loc(),
                                        "Unexpected {}, expected register \"x\" or \"y\"",
                                        tok.as_display(&asm.str_interner)
                                    );
                                }
                            }
                        } else {
                            asm.data.push(0x8D);
                        }
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Stx => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    asm.next()?;
                                    asm.expect_register(RegisterName::Y)?;
                                    asm.data.push(0x96);
                                } else {
                                    asm.data.push(0x86);
                                }
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        asm.data.push(0x8E);
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Sty => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(_) => {
                        let (loc, expr) = asm.expr()?;
                        let value = if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) <= (u8::MAX as u32) {
                                if asm.peeked_symbol(SymbolName::Comma)?.is_some() {
                                    asm.next()?;
                                    asm.expect_register(RegisterName::X)?;
                                    asm.data.push(0x94);
                                } else {
                                    asm.data.push(0x84);
                                }
                                asm.data.push(value as u8);
                                return Ok(());
                            }
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            value
                        } else {
                            // We need to add 1 since we havent written the opcode yet :|
                            asm.links.push(Link::word(loc, asm.data.len() + 1, expr));
                            0
                        };

                        asm.data.push(0x8C);
                        asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                    }
                }
            }

            OperationName::Tax => {
                asm.next()?;
                asm.data.push(0xAA);
            }

            OperationName::Tay => {
                asm.next()?;
                asm.data.push(0xA8);
            }

            OperationName::Tsx => {
                asm.next()?;
                asm.data.push(0xBA);
            }

            OperationName::Txa => {
                asm.next()?;
                asm.data.push(0x8A);
            }

            OperationName::Txs => {
                asm.next()?;
                asm.data.push(0x9A);
            }

            OperationName::Tya => {
                asm.next()?;
                asm.data.push(0x98);
            }
        }
        Ok(())
    }
}

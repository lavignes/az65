use std::{
    fmt::{self, Display, Formatter},
    io::Read,
};

use crate::{
    assembler::{ArchAssembler, Assembler, AssemblerError},
    expr::ExprNode,
    fileman::FileSystem,
    lexer::{self, ArchTokens, SourceLoc, SymbolName, Token},
    linker::Link,
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
    Adc,
    Add,
    And,
    Bit,
    Call,
    Ccf,
    Cp,
    Cpd,
    Cpdr,
    Cpi,
    Cpir,
    Cpl,
    Daa,
    Dec,
    Di,
    Djnz,
    Ei,
    Ex,
    Exx,
    Halt,
    Im,
    In,
    Inc,
    Ind,
    Indr,
    Ini,
    Inir,
    Jp,
    Jr,
    Ld,
    Ldd,
    Lddr,
    Ldi,
    Ldir,
    Neg,
    Nop,
    Or,
    Otdr,
    Otir,
    Out,
    Outd,
    Outi,
    Pop,
    Push,
    Res,
    Ret,
    Reti,
    Retn,
    Rl,
    Rla,
    Rlc,
    Rlca,
    Rld,
    Rr,
    Rra,
    Rrc,
    Rrca,
    Rrd,
    Rst,
    Sbc,
    Scf,
    Set,
    Sla,
    Sll,
    Sra,
    Srl,
    Sub,
    Xor,
}

impl lexer::OperationName for OperationName {
    fn parse<S: AsRef<str>>(s: S) -> Option<Self> {
        match s.as_ref() {
            "adc" | "ADC" => Some(Self::Adc),
            "add" | "ADD" => Some(Self::Add),
            "and" | "AND" => Some(Self::And),
            "bit" | "BIT" => Some(Self::Bit),
            "call" | "CALL" => Some(Self::Call),
            "ccf" | "CCF" => Some(Self::Ccf),
            "cp" | "CP" => Some(Self::Cp),
            "cpd" | "CPD" => Some(Self::Cpd),
            "cpdr" | "CPDR" => Some(Self::Cpdr),
            "cpi" | "CPI" => Some(Self::Cpi),
            "cpir" | "CPIR" => Some(Self::Cpir),
            "cpl" | "CPL" => Some(Self::Cpl),
            "daa" | "DAA" => Some(Self::Daa),
            "dec" | "DEC" => Some(Self::Dec),
            "di" | "DI" => Some(Self::Di),
            "djnz" | "DJNZ" => Some(Self::Djnz),
            "ei" | "EI" => Some(Self::Ei),
            "ex" | "EX" => Some(Self::Ex),
            "exx" | "EXX" => Some(Self::Exx),
            "halt" | "HALT" => Some(Self::Halt),
            "im" | "IM" => Some(Self::Im),
            "in" | "IN" => Some(Self::In),
            "inc" | "INC" => Some(Self::Inc),
            "ind" | "IND" => Some(Self::Ind),
            "indr" | "INDR" => Some(Self::Indr),
            "ini" | "INI" => Some(Self::Ini),
            "inir" | "INIR" => Some(Self::Inir),
            "jp" | "JP" => Some(Self::Jp),
            "jr" | "JR" => Some(Self::Jr),
            "ld" | "LD" => Some(Self::Ld),
            "ldd" | "LDD" => Some(Self::Ldd),
            "lddr" | "LDDR" => Some(Self::Lddr),
            "ldi" | "LDI" => Some(Self::Ldi),
            "ldir" | "LDIR" => Some(Self::Ldir),
            "neg" | "NEG" => Some(Self::Neg),
            "nop" | "NOP" => Some(Self::Nop),
            "or" | "OR" => Some(Self::Or),
            "otdr" | "OTDR" => Some(Self::Otdr),
            "otir" | "OTIR" => Some(Self::Otir),
            "out" | "OUT" => Some(Self::Out),
            "outd" | "OUTD" => Some(Self::Outd),
            "outi" | "OUTI" => Some(Self::Outi),
            "pop" | "POP" => Some(Self::Pop),
            "push" | "PUSH" => Some(Self::Push),
            "res" | "RES" => Some(Self::Res),
            "ret" | "RET" => Some(Self::Ret),
            "reti" | "RETI" => Some(Self::Reti),
            "retn" | "RETN" => Some(Self::Retn),
            "rl" | "RL" => Some(Self::Rl),
            "rla" | "RLA" => Some(Self::Rla),
            "rlc" | "RLC" => Some(Self::Rlc),
            "rlca" | "RLCA" => Some(Self::Rlca),
            "rld" | "RLD" => Some(Self::Rld),
            "rr" | "RR" => Some(Self::Rr),
            "rra" | "RRA" => Some(Self::Rra),
            "rrc" | "RRC" => Some(Self::Rrc),
            "rrca" | "RRCA" => Some(Self::Rrca),
            "rrd" | "RRD" => Some(Self::Rrd),
            "rst" | "RST" => Some(Self::Rst),
            "sbc" | "SBC" => Some(Self::Sbc),
            "scf" | "SCF" => Some(Self::Scf),
            "set" | "SET" => Some(Self::Set),
            "sla" | "SLA" => Some(Self::Sla),
            "sll" | "SLL" => Some(Self::Sll),
            "sra" | "SRA" => Some(Self::Sra),
            "srl" | "SRL" => Some(Self::Srl),
            "sub" | "SUB" => Some(Self::Sub),
            "xor" | "XOR" => Some(Self::Xor),
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
                Self::Add => "add",
                Self::And => "and",
                Self::Bit => "bit",
                Self::Call => "call",
                Self::Ccf => "ccf",
                Self::Cp => "cp",
                Self::Cpd => "cpd",
                Self::Cpdr => "cpdr",
                Self::Cpi => "cpi",
                Self::Cpir => "cpir",
                Self::Cpl => "cpl",
                Self::Daa => "daa",
                Self::Dec => "dec",
                Self::Di => "di",
                Self::Djnz => "djnz",
                Self::Ei => "ei",
                Self::Ex => "ex",
                Self::Exx => "exx",
                Self::Halt => "halt",
                Self::Im => "im",
                Self::In => "in",
                Self::Inc => "inc",
                Self::Ind => "ind",
                Self::Indr => "indr",
                Self::Ini => "ini",
                Self::Inir => "inir",
                Self::Jp => "jp",
                Self::Jr => "jr",
                Self::Ld => "ld",
                Self::Ldd => "ldd",
                Self::Lddr => "lddr",
                Self::Ldi => "ldi",
                Self::Ldir => "ldir",
                Self::Neg => "neg",
                Self::Nop => "nop",
                Self::Or => "or",
                Self::Otdr => "otdr",
                Self::Otir => "otir",
                Self::Out => "out",
                Self::Outd => "outd",
                Self::Outi => "outi",
                Self::Pop => "pop",
                Self::Push => "push",
                Self::Res => "res",
                Self::Ret => "ret",
                Self::Reti => "reti",
                Self::Retn => "retn",
                Self::Rl => "rl",
                Self::Rla => "rla",
                Self::Rlc => "rlc",
                Self::Rlca => "rlca",
                Self::Rld => "rld",
                Self::Rr => "rr",
                Self::Rra => "rra",
                Self::Rrc => "rrc",
                Self::Rrca => "rrca",
                Self::Rrd => "rrd",
                Self::Rst => "rst",
                Self::Sbc => "sbc",
                Self::Scf => "scf",
                Self::Set => "set",
                Self::Sla => "sla",
                Self::Sll => "sll",
                Self::Sra => "sra",
                Self::Srl => "srl",
                Self::Sub => "sub",
                Self::Xor => "xor",
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
    IX,
    IY,
    IXL,
    IXH,
    IYL,
    IYH,
    PC,
    SP,
    AFPrime,
    I,
    R,
}

impl lexer::RegisterName for RegisterName {
    fn parse<S: AsRef<str>>(s: S) -> Option<Self> {
        match s.as_ref() {
            "a" | "A" => Some(Self::A),
            "b" | "B" => Some(Self::B),
            "c" | "C" => Some(Self::C),
            "d" | "D" => Some(Self::D),
            "e" | "E" => Some(Self::E),
            "h" | "H" => Some(Self::H),
            "l" | "L" => Some(Self::L),
            "af" | "AF" => Some(Self::AF),
            "bc" | "BC" => Some(Self::BC),
            "de" | "DE" => Some(Self::DE),
            "hl" | "HL" => Some(Self::HL),
            "ix" | "IX" => Some(Self::IX),
            "iy" | "IY" => Some(Self::IY),
            "ixl" | "IXL" => Some(Self::IXL),
            "ixh" | "IXH" => Some(Self::IXH),
            "iyl" | "IYL" => Some(Self::IYL),
            "iyh" | "IYH" => Some(Self::IYH),
            "pc" | "PC" => Some(Self::PC),
            "sp" | "SP" => Some(Self::SP),
            "af'" | "AF'" => Some(Self::AFPrime),
            "i" | "I" => Some(Self::I),
            "r" | "R" => Some(Self::R),
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
                Self::IX => "ix",
                Self::IY => "iy",
                Self::IXL => "ixl",
                Self::IXH => "ixh",
                Self::IYL => "iyl",
                Self::IYH => "iyh",
                Self::PC => "pc",
                Self::SP => "sp",
                Self::AFPrime => "af'",
                Self::I => "i",
                Self::R => "r",
            }
        )
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum FlagName {
    Zero,
    NotZero,
    NotCarry,
    ParityEven,
    ParityOdd,
    Positive,
    Negative,
}

impl lexer::FlagName for FlagName {
    fn parse<S: AsRef<str>>(s: S) -> Option<Self> {
        match s.as_ref() {
            "z" | "Z" => Some(Self::Zero),
            "nz" | "NZ" => Some(Self::NotZero),
            "nc" | "NC" => Some(Self::NotCarry),
            "pe" | "PE" => Some(Self::ParityEven),
            "po" | "PO" => Some(Self::ParityOdd),
            "p" | "P" => Some(Self::Positive),
            "m" | "M" => Some(Self::Negative),
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
                Self::Zero => "zero flag",
                Self::NotZero => "not zero flag",
                Self::NotCarry => "not carry flag",
                Self::ParityEven => "parity even flag",
                Self::ParityOdd => "parity odd flag",
                Self::Positive => "positive flag",
                Self::Negative => "negative/minus flag",
            }
        )
    }
}

pub struct Z80;

#[derive(Copy, Clone)]
pub struct Z80Tokens;

impl ArchTokens for Z80Tokens {
    type RegisterName = RegisterName;
    type FlagName = FlagName;
    type OperationName = OperationName;
}

impl<S, R> ArchAssembler<S, R, Z80Tokens> for Z80
where
    S: FileSystem<Reader = R>,
    R: Read,
{
    fn parse(
        asm: &mut Assembler<S, R, Z80Tokens, Self>,
        name: OperationName,
    ) -> Result<(), (SourceLoc, AssemblerError)> {
        match name {
            OperationName::Adc => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),
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
                                asm.data.push(0x8F);
                            }

                            Some(Token::Register {
                                name: RegisterName::B,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x88);
                            }

                            Some(Token::Register {
                                name: RegisterName::C,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x89);
                            }

                            Some(Token::Register {
                                name: RegisterName::D,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x8A);
                            }

                            Some(Token::Register {
                                name: RegisterName::E,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x8B);
                            }

                            Some(Token::Register {
                                name: RegisterName::H,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x8C);
                            }

                            Some(Token::Register {
                                name: RegisterName::L,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x8D);
                            }

                            Some(Token::Register {
                                name: RegisterName::IXH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x8C);
                            }

                            Some(Token::Register {
                                name: RegisterName::IXL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x8D);
                            }

                            Some(Token::Register {
                                name: RegisterName::IYH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x8C);
                            }

                            Some(Token::Register {
                                name: RegisterName::IYL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x8D);
                            }

                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            }) => {
                                asm.next()?;
                                match asm.next()? {
                                    None => return asm.end_of_input_err(),

                                    Some(Token::Register {
                                        name: RegisterName::HL,
                                        ..
                                    }) => {
                                        asm.expect_symbol(SymbolName::ParenClose)?;
                                        asm.data.push(0x8E);
                                    }

                                    Some(Token::Register {
                                        name: RegisterName::IX,
                                        ..
                                    }) => {
                                        asm.expect_symbol(SymbolName::Plus)?;
                                        asm.data.push(0xDD);
                                        asm.data.push(0x8E);
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
                                        asm.expect_symbol(SymbolName::ParenClose)?;
                                    }

                                    Some(Token::Register {
                                        name: RegisterName::IY,
                                        ..
                                    }) => {
                                        asm.expect_symbol(SymbolName::Plus)?;
                                        asm.data.push(0xFD);
                                        asm.data.push(0x8E);
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
                                        asm.expect_symbol(SymbolName::ParenClose)?;
                                    }

                                    Some(_) => {
                                        asm.data.push(0xCE);
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
                                        asm.expect_symbol(SymbolName::ParenClose)?;
                                    }
                                }
                            }

                            Some(_) => {
                                asm.data.push(0xCE);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                            }
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::HL,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Register {
                                name: RegisterName::BC,
                                ..
                            }) => {
                                asm.data.push(0xED);
                                asm.data.push(0x4A);
                            }

                            Some(Token::Register {
                                name: RegisterName::DE,
                                ..
                            }) => {
                                asm.data.push(0xED);
                                asm.data.push(0x5A);
                            }

                            Some(Token::Register {
                                name: RegisterName::HL,
                                ..
                            }) => {
                                asm.data.push(0xED);
                                asm.data.push(0x6A);
                            }

                            Some(Token::Register {
                                name: RegisterName::SP,
                                ..
                            }) => {
                                asm.data.push(0xED);
                                asm.data.push(0x7A);
                            }

                            Some(tok) => {
                                return asm_err!(
                                tok.loc(),
                                "Unexpected {}, expected register \"bc\", \"de\", \"hl\" or \"sp\"",
                                tok.as_display(&asm.str_interner)
                            )
                            }
                        }
                    }

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected register \"a\" or \"hl\"",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                }
            }

            OperationName::Add => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),
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
                                asm.data.push(0x87);
                            }

                            Some(Token::Register {
                                name: RegisterName::B,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x80);
                            }

                            Some(Token::Register {
                                name: RegisterName::C,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x81);
                            }

                            Some(Token::Register {
                                name: RegisterName::D,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x82);
                            }

                            Some(Token::Register {
                                name: RegisterName::E,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x83);
                            }

                            Some(Token::Register {
                                name: RegisterName::H,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x84);
                            }

                            Some(Token::Register {
                                name: RegisterName::L,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x85);
                            }

                            Some(Token::Register {
                                name: RegisterName::IXH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x84);
                            }

                            Some(Token::Register {
                                name: RegisterName::IXL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x85);
                            }

                            Some(Token::Register {
                                name: RegisterName::IYH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x84);
                            }

                            Some(Token::Register {
                                name: RegisterName::IYL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x85);
                            }

                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            }) => {
                                asm.next()?;
                                match asm.next()? {
                                    None => return asm.end_of_input_err(),
                                    Some(Token::Register {
                                        name: RegisterName::HL,
                                        ..
                                    }) => {
                                        asm.data.push(0x86);
                                        asm.expect_symbol(SymbolName::ParenClose)?;
                                    }

                                    Some(Token::Register {
                                        name: RegisterName::IX,
                                        ..
                                    }) => {
                                        asm.expect_symbol(SymbolName::Plus)?;
                                        asm.data.push(0xDD);
                                        asm.data.push(0x86);
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
                                        asm.expect_symbol(SymbolName::ParenClose)?;
                                    }

                                    Some(Token::Register {
                                        name: RegisterName::IY,
                                        ..
                                    }) => {
                                        asm.expect_symbol(SymbolName::Plus)?;
                                        asm.data.push(0xFD);
                                        asm.data.push(0x86);
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
                                        asm.expect_symbol(SymbolName::ParenClose)?;
                                    }

                                    Some(_) => {
                                        asm.data.push(0xC6);
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
                                        asm.expect_symbol(SymbolName::ParenClose)?;
                                    }
                                }
                            }

                            Some(_) => {
                                asm.data.push(0xC6);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                            }
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::HL,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Register {
                                name: RegisterName::BC,
                                ..
                            }) => {
                                asm.data.push(0x09);
                            }

                            Some(Token::Register {
                                name: RegisterName::DE,
                                ..
                            }) => {
                                asm.data.push(0x19);
                            }

                            Some(Token::Register {
                                name: RegisterName::HL,
                                ..
                            }) => {
                                asm.data.push(0x29);
                            }

                            Some(Token::Register {
                                name: RegisterName::SP,
                                ..
                            }) => {
                                asm.data.push(0x39);
                            }

                            Some(tok) => {
                                return asm_err!(
                                tok.loc(),
                                "Unexpected {}, expected register \"bc\", \"de\", \"hl\" or \"sp\"",
                                tok.as_display(&asm.str_interner)
                            )
                            }
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::IX,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Register {
                                name: RegisterName::BC,
                                ..
                            }) => {
                                asm.data.push(0xDD);
                                asm.data.push(0x09);
                            }

                            Some(Token::Register {
                                name: RegisterName::DE,
                                ..
                            }) => {
                                asm.data.push(0xDD);
                                asm.data.push(0x19);
                            }

                            Some(Token::Register {
                                name: RegisterName::IX,
                                ..
                            }) => {
                                asm.data.push(0xDD);
                                asm.data.push(0x29);
                            }

                            Some(Token::Register {
                                name: RegisterName::SP,
                                ..
                            }) => {
                                asm.data.push(0xDD);
                                asm.data.push(0x39);
                            }

                            Some(tok) => {
                                return asm_err!(
                                tok.loc(),
                                "Unexpected {}, expected register \"bc\", \"de\", \"ix\" or \"sp\"",
                                tok.as_display(&asm.str_interner)
                            )
                            }
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::IY,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Register {
                                name: RegisterName::BC,
                                ..
                            }) => {
                                asm.data.push(0xFD);
                                asm.data.push(0x09);
                            }

                            Some(Token::Register {
                                name: RegisterName::DE,
                                ..
                            }) => {
                                asm.data.push(0xFD);
                                asm.data.push(0x19);
                            }

                            Some(Token::Register {
                                name: RegisterName::IY,
                                ..
                            }) => {
                                asm.data.push(0xFD);
                                asm.data.push(0x29);
                            }

                            Some(Token::Register {
                                name: RegisterName::SP,
                                ..
                            }) => {
                                asm.data.push(0xFD);
                                asm.data.push(0x39);
                            }

                            Some(tok) => {
                                return asm_err!(
                                tok.loc(),
                                "Unexpected {}, expected register \"bc\", \"de\", \"iy\" or \"sp\"",
                                tok.as_display(&asm.str_interner)
                            )
                            }
                        }
                    }

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected register \"a\" or \"hl\"",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                }
            }

            OperationName::And => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),
                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xA7);
                    }

                    Some(Token::Register {
                        name: RegisterName::B,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xA0);
                    }

                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xA1);
                    }

                    Some(Token::Register {
                        name: RegisterName::D,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xA2);
                    }

                    Some(Token::Register {
                        name: RegisterName::E,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xA3);
                    }

                    Some(Token::Register {
                        name: RegisterName::H,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xA4);
                    }

                    Some(Token::Register {
                        name: RegisterName::L,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xA5);
                    }

                    Some(Token::Register {
                        name: RegisterName::IXH,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xDD);
                        asm.data.push(0xA4);
                    }

                    Some(Token::Register {
                        name: RegisterName::IXL,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xDD);
                        asm.data.push(0xA5);
                    }

                    Some(Token::Register {
                        name: RegisterName::IYH,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xFD);
                        asm.data.push(0xA4);
                    }

                    Some(Token::Register {
                        name: RegisterName::IYL,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xFD);
                        asm.data.push(0xA5);
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.next()?;
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Register {
                                name: RegisterName::HL,
                                ..
                            }) => {
                                asm.data.push(0xA6);
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(Token::Register {
                                name: RegisterName::IX,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xDD);
                                asm.data.push(0xA6);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(Token::Register {
                                name: RegisterName::IY,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xFD);
                                asm.data.push(0xA6);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected registers \"hl\", \"ix\", or \"iy\"",
                                    tok.as_display(&asm.str_interner)
                                )
                            }
                        }
                    }

                    Some(_) => {
                        asm.data.push(0xE6);
                        let (loc, expr) = asm.expr()?;
                        if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u8::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a byte"
                                );
                            }
                            asm.data.push(value as u8);
                        } else {
                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                            asm.data.push(0);
                        }
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
                                match asm.next()? {
                                    Some(Token::Register { name: RegisterName::HL, .. }) => {
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

                                    Some(Token::Register { name: RegisterName::IX, .. }) => {
                                        asm.expect_symbol(SymbolName::Plus)?;
                                        asm.data.push(0xDD);
                                        asm.data.push(0xCB);
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
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

                                    Some(Token::Register { name: RegisterName::IY, .. }) => {
                                        asm.expect_symbol(SymbolName::Plus)?;
                                        asm.data.push(0xFD);
                                        asm.data.push(0xCB);
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
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

                                    Some(tok) => return asm_err!(tok.loc(),"Unexpected {}, expected register \"hl\", \"ix\", or \"iy\"",tok.as_display(&asm.str_interner)),
                                    None => return asm.end_of_input_err(),
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected a register",
                                    tok.as_display(&asm.str_interner)
                                )
                            }
                            None => return asm.end_of_input_err(),
                        }
                    }
                }
            }

            OperationName::Call => {
                asm.next()?;
                match asm.peek()? {
                    Some(Token::Flag {
                        name: FlagName::Zero,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xCC);
                        asm.expect_symbol(SymbolName::Comma)?;
                    }
                    Some(Token::Flag {
                        name: FlagName::NotZero,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xC4);
                        asm.expect_symbol(SymbolName::Comma)?;
                    }
                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xDC);
                        asm.expect_symbol(SymbolName::Comma)?;
                    }
                    Some(Token::Flag {
                        name: FlagName::NotCarry,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xD4);
                        asm.expect_symbol(SymbolName::Comma)?;
                    }
                    Some(Token::Flag {
                        name: FlagName::ParityEven,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xEC);
                        asm.expect_symbol(SymbolName::Comma)?;
                    }
                    Some(Token::Flag {
                        name: FlagName::ParityOdd,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xE4);
                        asm.expect_symbol(SymbolName::Comma)?;
                    }
                    Some(Token::Flag {
                        name: FlagName::Positive,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xF4);
                        asm.expect_symbol(SymbolName::Comma)?;
                    }
                    Some(Token::Flag {
                        name: FlagName::Negative,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xFC);
                        asm.expect_symbol(SymbolName::Comma)?;
                    }
                    Some(_) => {
                        asm.data.push(0xCD);
                    }
                    None => return asm.end_of_input_err(),
                }
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
                }
            }

            OperationName::Ccf => {
                asm.next()?;
                asm.data.push(0x3F);
            }

            OperationName::Cp => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),
                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xBF);
                    }

                    Some(Token::Register {
                        name: RegisterName::B,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xB8);
                    }

                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xB9);
                    }

                    Some(Token::Register {
                        name: RegisterName::D,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xBA);
                    }

                    Some(Token::Register {
                        name: RegisterName::E,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xBB);
                    }

                    Some(Token::Register {
                        name: RegisterName::H,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xBC);
                    }

                    Some(Token::Register {
                        name: RegisterName::L,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xBD);
                    }

                    Some(Token::Register {
                        name: RegisterName::IXH,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xDD);
                        asm.data.push(0xBC);
                    }

                    Some(Token::Register {
                        name: RegisterName::IXL,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xDD);
                        asm.data.push(0xBD);
                    }

                    Some(Token::Register {
                        name: RegisterName::IYH,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xFD);
                        asm.data.push(0xBC);
                    }

                    Some(Token::Register {
                        name: RegisterName::IYL,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xFD);
                        asm.data.push(0xBD);
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.next()?;
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Register {
                                name: RegisterName::HL,
                                ..
                            }) => {
                                asm.data.push(0xBE);
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(Token::Register {
                                name: RegisterName::IX,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xDD);
                                asm.data.push(0xBE);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(Token::Register {
                                name: RegisterName::IY,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xFD);
                                asm.data.push(0xBE);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(_) => {
                                asm.data.push(0xFE);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }
                        }
                    }

                    Some(_) => {
                        asm.data.push(0xFE);
                        let (loc, expr) = asm.expr()?;
                        if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u8::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a byte"
                                );
                            }
                            asm.data.push(value as u8);
                        } else {
                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                            asm.data.push(0);
                        }
                    }
                }
            }

            OperationName::Cpd => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0xA9);
            }

            OperationName::Cpdr => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0xB9);
            }

            OperationName::Cpi => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0xA1);
            }

            OperationName::Cpir => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0xB1);
            }

            OperationName::Cpl => {
                asm.next()?;
                asm.data.push(0x2F);
            }

            OperationName::Daa => {
                asm.next()?;
                asm.data.push(0x27);
            }

            OperationName::Dec => {
                asm.next()?;
                match asm.next()? {
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

                    Some(Token::Register {
                        name: RegisterName::IXH,
                        ..
                    }) => {
                        asm.data.push(0xDD);
                        asm.data.push(0x25);
                    }

                    Some(Token::Register {
                        name: RegisterName::IXL,
                        ..
                    }) => {
                        asm.data.push(0xDD);
                        asm.data.push(0x2D);
                    }

                    Some(Token::Register {
                        name: RegisterName::IYH,
                        ..
                    }) => {
                        asm.data.push(0xFD);
                        asm.data.push(0x25);
                    }

                    Some(Token::Register {
                        name: RegisterName::IYL,
                        ..
                    }) => {
                        asm.data.push(0xFD);
                        asm.data.push(0x2D);
                    }

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
                        name: RegisterName::IX,
                        ..
                    }) => {
                        asm.data.push(0xDD);
                        asm.data.push(0x2B);
                    }

                    Some(Token::Register {
                        name: RegisterName::IY,
                        ..
                    }) => {
                        asm.data.push(0xFD);
                        asm.data.push(0x2B);
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => match asm.next()? {
                        None => return asm.end_of_input_err(),

                        Some(Token::Register {
                            name: RegisterName::HL,
                            ..
                        }) => {
                            asm.data.push(0x35);
                            asm.expect_symbol(SymbolName::ParenClose)?;
                        }

                        Some(Token::Register {
                            name: RegisterName::IX,
                            ..
                        }) => {
                            asm.expect_symbol(SymbolName::Plus)?;
                            asm.data.push(0xDD);
                            asm.data.push(0x35);
                            let (loc, expr) = asm.expr()?;
                            if let Some(value) = expr.evaluate(&asm.symtab) {
                                if (value as u32) > (u8::MAX as u32) {
                                    return asm_err!(
                                        loc,
                                        "Expression result ({value}) will not fit in a byte"
                                    );
                                }
                                asm.data.push(value as u8);
                            } else {
                                asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                asm.data.push(0);
                            }
                            asm.expect_symbol(SymbolName::ParenClose)?;
                        }

                        Some(Token::Register {
                            name: RegisterName::IY,
                            ..
                        }) => {
                            asm.expect_symbol(SymbolName::Plus)?;
                            asm.data.push(0xFD);
                            asm.data.push(0x35);
                            let (loc, expr) = asm.expr()?;
                            if let Some(value) = expr.evaluate(&asm.symtab) {
                                if (value as u32) > (u8::MAX as u32) {
                                    return asm_err!(
                                        loc,
                                        "Expression result ({value}) will not fit in a byte"
                                    );
                                }
                                asm.data.push(value as u8);
                            } else {
                                asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                asm.data.push(0);
                            }
                            asm.expect_symbol(SymbolName::ParenClose)?;
                        }

                        Some(tok) => {
                            return asm_err!(
                                tok.loc(),
                                "Unexpected {}, expected registers \"hl\", \"ix\", or \"iy\"",
                                tok.as_display(&asm.str_interner)
                            )
                        }
                    },

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected a register",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                    None => return asm.end_of_input_err(),
                }
            }

            OperationName::Di => {
                asm.next()?;
                asm.data.push(0xF3);
            }

            OperationName::Djnz => {
                asm.next()?;
                asm.data.push(0x10);
                let (loc, mut expr) = asm.expr()?;
                // Make the expression relative to @here
                expr.push(ExprNode::Value(asm.here.wrapping_add(2) as i32));
                expr.push(ExprNode::Sub);
                if let Some(value) = expr.evaluate(&asm.symtab) {
                    if (value < (i8::MIN as i32)) || (value > (i8::MAX as i32)) {
                        return asm_err!(loc, "Jump distance ({value}) will not fit in a byte");
                    }
                    asm.data.push(value as u8);
                } else {
                    asm.links.push(Link::signed_byte(loc, asm.data.len(), expr));
                    asm.data.push(0);
                }
            }

            OperationName::Ei => {
                asm.next()?;
                asm.data.push(0xFB);
            }

            OperationName::Ex => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register {
                        name: RegisterName::AF,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.expect_register(RegisterName::AFPrime)?;
                        asm.data.push(0x08);
                    }

                    Some(Token::Register {
                        name: RegisterName::DE,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.expect_register(RegisterName::HL)?;
                        asm.data.push(0xEB);
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.expect_register(RegisterName::SP)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                        asm.expect_symbol(SymbolName::Comma)?;

                        match asm.next()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Register {
                                name: RegisterName::HL,
                                ..
                            }) => {
                                asm.data.push(0xE3);
                            }

                            Some(Token::Register {
                                name: RegisterName::IX,
                                ..
                            }) => {
                                asm.data.push(0xDD);
                                asm.data.push(0xE3);
                            }

                            Some(Token::Register {
                                name: RegisterName::IY,
                                ..
                            }) => {
                                asm.data.push(0xFD);
                                asm.data.push(0xE3);
                            }

                            Some(tok) => {
                                return asm_err!(
                                tok.loc(),
                                "Unexpected {}, expected the registers \"hl\", \"ix\", or \"iy\"",
                                tok.as_display(&asm.str_interner)
                            )
                            }
                        }
                    }

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected the registers \"af\", \"de\", or \"(sp)\"",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                }
            }

            OperationName::Exx => {
                asm.next()?;
                asm.data.push(0xD9);
            }

            OperationName::Halt => {
                asm.next()?;
                asm.data.push(0x76);
            }

            OperationName::Im => {
                asm.next()?;
                asm.data.push(0xED);
                match asm.next()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Number { value: 0, .. }) => {
                        asm.data.push(0x46);
                    }

                    Some(Token::Number { value: 1, .. }) => {
                        asm.data.push(0x56);
                    }

                    Some(Token::Number { value: 2, .. }) => {
                        asm.data.push(0x5E);
                    }

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected the numbers 0, 1, or 2",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                }
            }

            OperationName::In => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.expect_symbol(SymbolName::ParenOpen)?;
                        match asm.peek()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Register {
                                name: RegisterName::C,
                                ..
                            }) => {
                                asm.next()?;
                                asm.expect_symbol(SymbolName::ParenClose)?;
                                asm.data.push(0xED);
                                asm.data.push(0x78);
                            }

                            Some(_) => {
                                asm.data.push(0xDB);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::B,
                        ..
                    }) => {
                        asm.data.push(0xED);
                        asm.data.push(0x40);
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.expect_symbol(SymbolName::ParenOpen)?;
                        asm.expect_register(RegisterName::C)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                    }

                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.data.push(0xED);
                        asm.data.push(0x48);
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.expect_symbol(SymbolName::ParenOpen)?;
                        asm.expect_register(RegisterName::C)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                    }

                    Some(Token::Register {
                        name: RegisterName::D,
                        ..
                    }) => {
                        asm.data.push(0xED);
                        asm.data.push(0x50);
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.expect_symbol(SymbolName::ParenOpen)?;
                        asm.expect_register(RegisterName::C)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                    }

                    Some(Token::Register {
                        name: RegisterName::E,
                        ..
                    }) => {
                        asm.data.push(0xED);
                        asm.data.push(0x58);
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.expect_symbol(SymbolName::ParenOpen)?;
                        asm.expect_register(RegisterName::C)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                    }

                    Some(Token::Register {
                        name: RegisterName::H,
                        ..
                    }) => {
                        asm.data.push(0xED);
                        asm.data.push(0x60);
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.expect_symbol(SymbolName::ParenOpen)?;
                        asm.expect_register(RegisterName::C)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                    }

                    Some(Token::Register {
                        name: RegisterName::L,
                        ..
                    }) => {
                        asm.data.push(0xED);
                        asm.data.push(0x68);
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.expect_symbol(SymbolName::ParenOpen)?;
                        asm.expect_register(RegisterName::C)?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
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

            OperationName::Inc => {
                asm.next()?;
                match asm.next()? {
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

                    Some(Token::Register {
                        name: RegisterName::IXH,
                        ..
                    }) => {
                        asm.data.push(0xDD);
                        asm.data.push(0x24);
                    }

                    Some(Token::Register {
                        name: RegisterName::IXL,
                        ..
                    }) => {
                        asm.data.push(0xDD);
                        asm.data.push(0x2C);
                    }

                    Some(Token::Register {
                        name: RegisterName::IYH,
                        ..
                    }) => {
                        asm.data.push(0xFD);
                        asm.data.push(0x24);
                    }

                    Some(Token::Register {
                        name: RegisterName::IYL,
                        ..
                    }) => {
                        asm.data.push(0xFD);
                        asm.data.push(0x2C);
                    }

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
                        name: RegisterName::IX,
                        ..
                    }) => {
                        asm.data.push(0xDD);
                        asm.data.push(0x23);
                    }

                    Some(Token::Register {
                        name: RegisterName::IY,
                        ..
                    }) => {
                        asm.data.push(0xFD);
                        asm.data.push(0x23);
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => match asm.next()? {
                        None => return asm.end_of_input_err(),

                        Some(Token::Register {
                            name: RegisterName::HL,
                            ..
                        }) => {
                            asm.data.push(0x34);
                            asm.expect_symbol(SymbolName::ParenClose)?;
                        }

                        Some(Token::Register {
                            name: RegisterName::IX,
                            ..
                        }) => {
                            asm.expect_symbol(SymbolName::Plus)?;
                            asm.data.push(0xDD);
                            asm.data.push(0x34);
                            let (loc, expr) = asm.expr()?;
                            if let Some(value) = expr.evaluate(&asm.symtab) {
                                if (value as u32) > (u8::MAX as u32) {
                                    return asm_err!(
                                        loc,
                                        "Expression result ({value}) will not fit in a byte"
                                    );
                                }
                                asm.data.push(value as u8);
                            } else {
                                asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                asm.data.push(0);
                            }
                            asm.expect_symbol(SymbolName::ParenClose)?;
                        }

                        Some(Token::Register {
                            name: RegisterName::IY,
                            ..
                        }) => {
                            asm.expect_symbol(SymbolName::Plus)?;
                            asm.data.push(0xFD);
                            asm.data.push(0x34);
                            let (loc, expr) = asm.expr()?;
                            if let Some(value) = expr.evaluate(&asm.symtab) {
                                if (value as u32) > (u8::MAX as u32) {
                                    return asm_err!(
                                        loc,
                                        "Expression result ({value}) will not fit in a byte"
                                    );
                                }
                                asm.data.push(value as u8);
                            } else {
                                asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                asm.data.push(0);
                            }
                            asm.expect_symbol(SymbolName::ParenClose)?;
                        }

                        Some(tok) => {
                            return asm_err!(
                                tok.loc(),
                                "Unexpected {}, expected registers \"hl\", \"ix\", or \"iy\"",
                                tok.as_display(&asm.str_interner)
                            )
                        }
                    },

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected a register",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                    None => return asm.end_of_input_err(),
                }
            }

            OperationName::Ind => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0xAA);
            }

            OperationName::Indr => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0xBA);
            }

            OperationName::Ini => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0xA2);
            }

            OperationName::Inir => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0xB2);
            }

            OperationName::Jp => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.next()?;
                        match asm.next()? {
                            None => return asm.end_of_input_err(),

                            Some(Token::Register {
                                name: RegisterName::HL,
                                ..
                            }) => {
                                asm.data.push(0xE9);
                            }

                            Some(Token::Register {
                                name: RegisterName::IX,
                                ..
                            }) => {
                                asm.data.push(0xDD);
                                asm.data.push(0xE9);
                            }

                            Some(Token::Register {
                                name: RegisterName::IY,
                                ..
                            }) => {
                                asm.data.push(0xFD);
                                asm.data.push(0xE9);
                            }

                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected register \"hl\", \"ix\", or \"iy\"",
                                    tok.as_display(&asm.str_interner)
                                )
                            }
                        }
                        asm.expect_symbol(SymbolName::ParenClose)?;
                    }

                    Some(_) => {
                        match asm.peek()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Flag {
                                name: FlagName::Zero,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xCA);
                                asm.expect_symbol(SymbolName::Comma)?;
                            }
                            Some(Token::Flag {
                                name: FlagName::NotZero,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xC2);
                                asm.expect_symbol(SymbolName::Comma)?;
                            }
                            Some(Token::Register {
                                name: RegisterName::C,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDA);
                                asm.expect_symbol(SymbolName::Comma)?;
                            }
                            Some(Token::Flag {
                                name: FlagName::NotCarry,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xD2);
                                asm.expect_symbol(SymbolName::Comma)?;
                            }
                            Some(Token::Flag {
                                name: FlagName::ParityEven,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xEA);
                                asm.expect_symbol(SymbolName::Comma)?;
                            }
                            Some(Token::Flag {
                                name: FlagName::ParityOdd,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xE2);
                                asm.expect_symbol(SymbolName::Comma)?;
                            }
                            Some(Token::Flag {
                                name: FlagName::Positive,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xF2);
                                asm.expect_symbol(SymbolName::Comma)?;
                            }
                            Some(Token::Flag {
                                name: FlagName::Negative,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFA);
                                asm.expect_symbol(SymbolName::Comma)?;
                            }
                            Some(_) => {
                                asm.data.push(0xC3);
                            }
                        }
                        let (loc, expr) = asm.expr()?;
                        if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                        } else {
                            asm.links.push(Link::word(loc, asm.data.len(), expr));
                            asm.data.push(0);
                            asm.data.push(0);
                        }
                    }
                }
            }

            OperationName::Jr => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Flag {
                        name: FlagName::NotZero,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0x20);
                        asm.expect_symbol(SymbolName::Comma)?;
                    }

                    Some(Token::Flag {
                        name: FlagName::Zero,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0x28);
                        asm.expect_symbol(SymbolName::Comma)?;
                    }

                    Some(Token::Flag {
                        name: FlagName::NotCarry,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0x30);
                        asm.expect_symbol(SymbolName::Comma)?;
                    }

                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0x38);
                        asm.expect_symbol(SymbolName::Comma)?;
                    }

                    Some(_) => {
                        asm.data.push(0x18);
                    }
                }
                let (loc, mut expr) = asm.expr()?;
                // Make the expression relative to @here
                expr.push(ExprNode::Value(asm.here.wrapping_add(2) as i32));
                expr.push(ExprNode::Sub);
                if let Some(value) = expr.evaluate(&asm.symtab) {
                    if (value < (i8::MIN as i32)) || (value > (i8::MAX as i32)) {
                        return asm_err!(loc, "Jump distance ({value}) will not fit in a byte");
                    }
                    asm.data.push(value as u8);
                } else {
                    asm.links.push(Link::signed_byte(loc, asm.data.len(), expr));
                    asm.data.push(0);
                }
            }

            OperationName::Ld => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),
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
                            Some(Token::Register {
                                name: RegisterName::IXH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x7C);
                            }
                            Some(Token::Register {
                                name: RegisterName::IXL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x7D);
                            }
                            Some(Token::Register {
                                name: RegisterName::IYH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x7C);
                            }
                            Some(Token::Register {
                                name: RegisterName::IYL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x7D);
                            }
                            Some(Token::Register {
                                name: RegisterName::I,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xED);
                                asm.data.push(0x57);
                            }
                            Some(Token::Register {
                                name: RegisterName::R,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xED);
                                asm.data.push(0x5F);
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
                                        asm.data.push(0x0A);
                                    }

                                    Some(Token::Register {
                                        name: RegisterName::DE,
                                        ..
                                    }) => {
                                        asm.next()?;
                                        asm.data.push(0x1A);
                                    }

                                    Some(Token::Register {
                                        name: RegisterName::HL,
                                        ..
                                    }) => {
                                        asm.next()?;
                                        asm.data.push(0x7E);
                                    }

                                    Some(Token::Register { loc, name }) => {
                                        asm.next()?;
                                        match name {
                                            RegisterName::IX => {
                                                asm.data.push(0xDD);
                                                asm.data.push(0x7E);
                                            }
                                            RegisterName::IY => {
                                                asm.data.push(0xFD);
                                                asm.data.push(0x7E);
                                            }
                                            _ => return asm_err!(loc,"Unexpected register \"{name}\", expected register \"ix\" or \"iy\""),
                                        }
                                        asm.expect_symbol(SymbolName::Plus)?;
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
                                    }

                                    Some(_) => {
                                        asm.data.push(0x3A);
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u16::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a word");
                                            }
                                            asm.data
                                                .extend_from_slice(&(value as u16).to_le_bytes());
                                        } else {
                                            asm.links.push(Link::word(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                            asm.data.push(0);
                                        }
                                    }
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }
                            Some(_) => {
                                asm.data.push(0x3E);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                            }
                        }
                    }

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
                            Some(Token::Register {
                                name: RegisterName::IXH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x44);
                            }
                            Some(Token::Register {
                                name: RegisterName::IXL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x45);
                            }
                            Some(Token::Register {
                                name: RegisterName::IYH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x44);
                            }
                            Some(Token::Register {
                                name: RegisterName::IYL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x45);
                            }
                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            }) => {
                                asm.next()?;
                                match asm.peek()? {
                                    None => return asm.end_of_input_err(),
                                    Some(Token::Register { name: RegisterName::HL, .. }) => {
                                        asm.next()?;
                                        asm.data.push(0x46);
                                    }
                                    Some(Token::Register { loc, name }) => {
                                        asm.next()?;
                                        match name {
                                            RegisterName::IX => {
                                                asm.data.push(0xDD);
                                                asm.data.push(0x46);
                                            }
                                            RegisterName::IY => {
                                                asm.data.push(0xFD);
                                                asm.data.push(0x46);
                                            }
                                            _ => return asm_err!(loc,"Unexpected register \"{name}\", expected register \"ix\" or \"iy\""),
                                        }
                                        asm.expect_symbol(SymbolName::Plus)?;
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
                                    }
                                    Some(tok) => return asm_err!(tok.loc() ,"Unexpected {}, expected registers \"hl\", \"ix\", or \"iy\"",tok.as_display(&asm.str_interner)),
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }
                            Some(_) => {
                                asm.data.push(0x06);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
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
                            Some(Token::Register {
                                name: RegisterName::IXH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x4C);
                            }
                            Some(Token::Register {
                                name: RegisterName::IXL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x4D);
                            }
                            Some(Token::Register {
                                name: RegisterName::IYH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x4C);
                            }
                            Some(Token::Register {
                                name: RegisterName::IYL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x4D);
                            }
                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            }) => {
                                asm.next()?;
                                match asm.peek()? {
                                    None => return asm.end_of_input_err(),
                                    Some(Token::Register { name: RegisterName::HL, .. }) => {
                                        asm.next()?;
                                        asm.data.push(0x4E);
                                    }
                                    Some(Token::Register { loc, name }) => {
                                        asm.next()?;
                                        match name {
                                            RegisterName::IX => {
                                                asm.data.push(0xDD);
                                                asm.data.push(0x4E);
                                            }
                                            RegisterName::IY => {
                                                asm.data.push(0xFD);
                                                asm.data.push(0x4E);
                                            }
                                            _ => return asm_err!(loc,"Unexpected register \"{name}\", expected register \"ix\" or \"iy\""),
                                        }
                                        asm.expect_symbol(SymbolName::Plus)?;
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
                                    }
                                    Some(tok) => return asm_err!(tok.loc(),"Unexpected {}, expected registers \"hl\", \"ix\", or \"iy\"",tok.as_display(&asm.str_interner)),
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }
                            Some(_) => {
                                asm.data.push(0x0E);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
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
                            Some(Token::Register {
                                name: RegisterName::IXH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x54);
                            }
                            Some(Token::Register {
                                name: RegisterName::IXL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x55);
                            }
                            Some(Token::Register {
                                name: RegisterName::IYH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x54);
                            }
                            Some(Token::Register {
                                name: RegisterName::IYL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x55);
                            }
                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            }) => {
                                asm.next()?;
                                match asm.peek()? {
                                    None => return asm.end_of_input_err(),
                                    Some(Token::Register { name: RegisterName::HL, .. }) => {
                                        asm.next()?;
                                        asm.data.push(0x56);
                                    }
                                    Some(Token::Register { loc, name }) => {
                                        asm.next()?;
                                        match name {
                                            RegisterName::IX => {
                                                asm.data.push(0xDD);
                                                asm.data.push(0x56);
                                            }
                                            RegisterName::IY => {
                                                asm.data.push(0xFD);
                                                asm.data.push(0x56);
                                            }
                                            _ => return asm_err!(loc,"Unexpected register \"{name}\", expected register \"ix\" or \"iy\""),
                                        }
                                        asm.expect_symbol(SymbolName::Plus)?;
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
                                    }
                                    Some(tok) => return asm_err!(tok.loc(),"Unexpected {}, expected registers \"hl\", \"ix\", or \"iy\"",tok.as_display(&asm.str_interner)),
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }
                            Some(_) => {
                                asm.data.push(0x16);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
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
                            Some(Token::Register {
                                name: RegisterName::IXH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x5C);
                            }
                            Some(Token::Register {
                                name: RegisterName::IXL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x5D);
                            }
                            Some(Token::Register {
                                name: RegisterName::IYH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x5C);
                            }
                            Some(Token::Register {
                                name: RegisterName::IYL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x5D);
                            }
                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            }) => {
                                asm.next()?;
                                match asm.peek()? {
                                    None => return asm.end_of_input_err(),
                                    Some(Token::Register { name: RegisterName::HL, .. }) => {
                                        asm.next()?;
                                        asm.data.push(0x5E);
                                    }
                                    Some(Token::Register { loc, name }) => {
                                        asm.next()?;
                                        match name {
                                            RegisterName::IX => {
                                                asm.data.push(0xDD);
                                                asm.data.push(0x5E);
                                            }
                                            RegisterName::IY => {
                                                asm.data.push(0xFD);
                                                asm.data.push(0x5E);
                                            }
                                            _ => return asm_err!(loc,"Unexpected register \"{name}\", expected register \"ix\" or \"iy\""),
                                        }
                                        asm.expect_symbol(SymbolName::Plus)?;
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
                                    }
                                    Some(tok) => return asm_err!(tok.loc(),"Unexpected {}, expected registers \"hl\", \"ix\", or \"iy\"",tok.as_display(&asm.str_interner)),
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }
                            Some(_) => {
                                asm.data.push(0x1E);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
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
                                match asm.peek()? {
                                    None => return asm.end_of_input_err(),
                                    Some(Token::Register { name: RegisterName::HL, .. }) => {
                                        asm.next()?;
                                        asm.data.push(0x66);
                                    }
                                    Some(Token::Register { loc, name }) => {
                                        asm.next()?;
                                        match name {
                                            RegisterName::IX => {
                                                asm.data.push(0xDD);
                                                asm.data.push(0x66);
                                            }
                                            RegisterName::IY => {
                                                asm.data.push(0xFD);
                                                asm.data.push(0x66);
                                            }
                                            _ => return asm_err!(loc,"Unexpected register \"{name}\", expected register \"ix\" or \"iy\""),
                                        }
                                        asm.expect_symbol(SymbolName::Plus)?;
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
                                    }
                                    Some(tok) => return asm_err!(tok.loc(),"Unexpected {}, expected registers \"hl\", \"ix\", or \"iy\"",tok.as_display(&asm.str_interner)),
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }
                            Some(_) => {
                                asm.data.push(0x26);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
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
                                match asm.peek()? {
                                    None => return asm.end_of_input_err(),
                                    Some(Token::Register { name: RegisterName::HL, .. }) => {
                                        asm.next()?;
                                        asm.data.push(0x6E);
                                    }
                                    Some(Token::Register { loc, name }) => {
                                        asm.next()?;
                                        match name {
                                            RegisterName::IX => {
                                                asm.data.push(0xDD);
                                                asm.data.push(0x6E);
                                            }
                                            RegisterName::IY => {
                                                asm.data.push(0xFD);
                                                asm.data.push(0x6E);
                                            }
                                            _ => return asm_err!(loc,"Unexpected register \"{name}\", expected register \"ix\" or \"iy\""),
                                        }
                                        asm.expect_symbol(SymbolName::Plus)?;
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
                                    }
                                    Some(tok) => return asm_err!(tok.loc(),"Unexpected {}, expected registers \"hl\", \"ix\", or \"iy\"",tok.as_display(&asm.str_interner)),
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }
                            Some(_) => {
                                asm.data.push(0x2E);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                            }
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::IXH,
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
                                asm.data.push(0xDD);
                                asm.data.push(0x67);
                            }
                            Some(Token::Register {
                                name: RegisterName::B,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x60);
                            }
                            Some(Token::Register {
                                name: RegisterName::C,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x61);
                            }
                            Some(Token::Register {
                                name: RegisterName::D,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x62);
                            }
                            Some(Token::Register {
                                name: RegisterName::E,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x63);
                            }
                            Some(Token::Register {
                                name: RegisterName::IXH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x64);
                            }
                            Some(Token::Register {
                                name: RegisterName::IXL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x65);
                            }
                            Some(_) => {
                                asm.data.push(0xDD);
                                asm.data.push(0x26);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                            }
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::IXL,
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
                                asm.data.push(0xDD);
                                asm.data.push(0x6F);
                            }
                            Some(Token::Register {
                                name: RegisterName::B,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x68);
                            }
                            Some(Token::Register {
                                name: RegisterName::C,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x69);
                            }
                            Some(Token::Register {
                                name: RegisterName::D,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x6A);
                            }
                            Some(Token::Register {
                                name: RegisterName::E,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x6B);
                            }
                            Some(Token::Register {
                                name: RegisterName::IXH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x6C);
                            }
                            Some(Token::Register {
                                name: RegisterName::IXL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x6D);
                            }
                            Some(_) => {
                                asm.data.push(0xDD);
                                asm.data.push(0x2E);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                            }
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::IYH,
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
                                asm.data.push(0xFD);
                                asm.data.push(0x67);
                            }
                            Some(Token::Register {
                                name: RegisterName::B,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x60);
                            }
                            Some(Token::Register {
                                name: RegisterName::C,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x61);
                            }
                            Some(Token::Register {
                                name: RegisterName::D,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x62);
                            }
                            Some(Token::Register {
                                name: RegisterName::E,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x63);
                            }
                            Some(Token::Register {
                                name: RegisterName::IYH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x64);
                            }
                            Some(Token::Register {
                                name: RegisterName::IYL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x65);
                            }
                            Some(_) => {
                                asm.data.push(0xFD);
                                asm.data.push(0x26);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                            }
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::IYL,
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
                                asm.data.push(0xFD);
                                asm.data.push(0x6F);
                            }
                            Some(Token::Register {
                                name: RegisterName::B,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x68);
                            }
                            Some(Token::Register {
                                name: RegisterName::C,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x69);
                            }
                            Some(Token::Register {
                                name: RegisterName::D,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x6A);
                            }
                            Some(Token::Register {
                                name: RegisterName::E,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x6B);
                            }
                            Some(Token::Register {
                                name: RegisterName::IYH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x6C);
                            }
                            Some(Token::Register {
                                name: RegisterName::IYL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x6D);
                            }
                            Some(_) => {
                                asm.data.push(0xFD);
                                asm.data.push(0x2E);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                            }
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::R,
                        ..
                    }) => {
                        asm.data.push(0xED);
                        asm.data.push(0x4F);
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.expect_register(RegisterName::A)?;
                    }

                    Some(Token::Register {
                        name: RegisterName::I,
                        ..
                    }) => {
                        asm.data.push(0xED);
                        asm.data.push(0x47);
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.expect_register(RegisterName::A)?;
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
                            Some(Token::Register {
                                name: RegisterName::IX,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0xF9);
                            }
                            Some(Token::Register {
                                name: RegisterName::IY,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0xF9);
                            }
                            Some(_) => {
                                let indirect = matches!(
                                    asm.peek()?,
                                    Some(Token::Symbol {
                                        name: SymbolName::ParenOpen,
                                        ..
                                    })
                                );
                                if indirect {
                                    asm.next()?;
                                    asm.data.push(0xED);
                                    asm.data.push(0x7B);
                                } else {
                                    asm.data.push(0x31);
                                }
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u16::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a word"
                                        );
                                    }
                                    asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                                } else {
                                    asm.links.push(Link::word(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                    asm.data.push(0);
                                }
                                if indirect {
                                    asm.expect_symbol(SymbolName::ParenClose)?;
                                }
                            }
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::BC,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        let indirect = matches!(
                            asm.peek()?,
                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            })
                        );
                        if indirect {
                            asm.next()?;
                            asm.data.push(0xED);
                            asm.data.push(0x4B);
                        } else {
                            asm.data.push(0x01);
                        }
                        let (loc, expr) = asm.expr()?;
                        if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                        } else {
                            asm.links.push(Link::word(loc, asm.data.len(), expr));
                            asm.data.push(0);
                            asm.data.push(0);
                        }
                        if indirect {
                            asm.expect_symbol(SymbolName::ParenClose)?;
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::DE,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        let indirect = matches!(
                            asm.peek()?,
                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            })
                        );
                        if indirect {
                            asm.next()?;
                            asm.data.push(0xED);
                            asm.data.push(0x5B);
                        } else {
                            asm.data.push(0x11);
                        }
                        let (loc, expr) = asm.expr()?;
                        if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                        } else {
                            asm.links.push(Link::word(loc, asm.data.len(), expr));
                            asm.data.push(0);
                            asm.data.push(0);
                        }
                        if indirect {
                            asm.expect_symbol(SymbolName::ParenClose)?;
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::HL,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        let indirect = matches!(
                            asm.peek()?,
                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            })
                        );
                        if indirect {
                            asm.next()?;
                            asm.data.push(0x2A);
                        } else {
                            asm.data.push(0x21);
                        }
                        let (loc, expr) = asm.expr()?;
                        if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                        } else {
                            asm.links.push(Link::word(loc, asm.data.len(), expr));
                            asm.data.push(0);
                            asm.data.push(0);
                        }
                        if indirect {
                            asm.expect_symbol(SymbolName::ParenClose)?;
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::IX,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.data.push(0xDD);
                        let indirect = matches!(
                            asm.peek()?,
                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            })
                        );
                        if indirect {
                            asm.next()?;
                            asm.data.push(0x2A);
                        } else {
                            asm.data.push(0x21);
                        }
                        let (loc, expr) = asm.expr()?;
                        if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                        } else {
                            asm.links.push(Link::word(loc, asm.data.len(), expr));
                            asm.data.push(0);
                            asm.data.push(0);
                        }
                        if indirect {
                            asm.expect_symbol(SymbolName::ParenClose)?;
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::IY,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.data.push(0xFD);
                        let indirect = matches!(
                            asm.peek()?,
                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            })
                        );
                        if indirect {
                            asm.next()?;
                            asm.data.push(0x2A);
                        } else {
                            asm.data.push(0x21);
                        }
                        let (loc, expr) = asm.expr()?;
                        if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u16::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a word"
                                );
                            }
                            asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                        } else {
                            asm.links.push(Link::word(loc, asm.data.len(), expr));
                            asm.data.push(0);
                            asm.data.push(0);
                        }
                        if indirect {
                            asm.expect_symbol(SymbolName::ParenClose)?;
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
                            name: RegisterName::HL,
                            ..
                        }) => {
                            asm.next()?;
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
                                    let (loc, expr) = asm.expr()?;
                                    if let Some(value) = expr.evaluate(&asm.symtab) {
                                        if (value as u32) > (u8::MAX as u32) {
                                            return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                        }
                                        asm.data.push(value as u8);
                                    } else {
                                        asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                        asm.data.push(0);
                                    }
                                }
                            }
                        }
                        Some(Token::Register {
                            name: RegisterName::IX,
                            ..
                        }) => {
                            asm.next()?;
                            asm.expect_symbol(SymbolName::Plus)?;
                            asm.data.push(0xDD);
                            let (loc, expr) = asm.expr()?;
                            let offset = if let Some(value) = expr.evaluate(&asm.symtab) {
                                if (value as u32) > (u8::MAX as u32) {
                                    return asm_err!(
                                        loc,
                                        "Expression result ({value}) will not fit in a byte"
                                    );
                                }
                                value as u8
                            } else {
                                asm.links.push(Link::byte(loc, asm.data.len() + 1, expr));
                                0
                            };
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
                                    asm.data.push(offset);
                                }
                                Some(Token::Register {
                                    name: RegisterName::B,
                                    ..
                                }) => {
                                    asm.next()?;
                                    asm.data.push(0x70);
                                    asm.data.push(offset);
                                }
                                Some(Token::Register {
                                    name: RegisterName::C,
                                    ..
                                }) => {
                                    asm.next()?;
                                    asm.data.push(0x71);
                                    asm.data.push(offset);
                                }
                                Some(Token::Register {
                                    name: RegisterName::D,
                                    ..
                                }) => {
                                    asm.next()?;
                                    asm.data.push(0x72);
                                    asm.data.push(offset);
                                }
                                Some(Token::Register {
                                    name: RegisterName::E,
                                    ..
                                }) => {
                                    asm.next()?;
                                    asm.data.push(0x73);
                                    asm.data.push(offset);
                                }
                                Some(Token::Register {
                                    name: RegisterName::H,
                                    ..
                                }) => {
                                    asm.next()?;
                                    asm.data.push(0x74);
                                    asm.data.push(offset);
                                }
                                Some(Token::Register {
                                    name: RegisterName::L,
                                    ..
                                }) => {
                                    asm.next()?;
                                    asm.data.push(0x75);
                                    asm.data.push(offset);
                                }
                                Some(_) => {
                                    asm.data.push(0x36);
                                    asm.data.push(offset);
                                    let (loc, expr) = asm.expr()?;
                                    if let Some(value) = expr.evaluate(&asm.symtab) {
                                        if (value as u32) > (u8::MAX as u32) {
                                            return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                        }
                                        asm.data.push(value as u8);
                                    } else {
                                        asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                        asm.data.push(0);
                                    }
                                }
                            }
                        }
                        Some(Token::Register {
                            name: RegisterName::IY,
                            ..
                        }) => {
                            asm.next()?;
                            asm.expect_symbol(SymbolName::Plus)?;
                            asm.data.push(0xFD);
                            let (loc, expr) = asm.expr()?;
                            let offset = if let Some(value) = expr.evaluate(&asm.symtab) {
                                if (value as u32) > (u8::MAX as u32) {
                                    return asm_err!(
                                        loc,
                                        "Expression result ({value}) will not fit in a byte"
                                    );
                                }
                                value as u8
                            } else {
                                asm.links.push(Link::byte(loc, asm.data.len() + 1, expr));
                                0
                            };
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
                                    asm.data.push(offset);
                                }
                                Some(Token::Register {
                                    name: RegisterName::B,
                                    ..
                                }) => {
                                    asm.next()?;
                                    asm.data.push(0x70);
                                    asm.data.push(offset);
                                }
                                Some(Token::Register {
                                    name: RegisterName::C,
                                    ..
                                }) => {
                                    asm.next()?;
                                    asm.data.push(0x71);
                                    asm.data.push(offset);
                                }
                                Some(Token::Register {
                                    name: RegisterName::D,
                                    ..
                                }) => {
                                    asm.next()?;
                                    asm.data.push(0x72);
                                    asm.data.push(offset);
                                }
                                Some(Token::Register {
                                    name: RegisterName::E,
                                    ..
                                }) => {
                                    asm.next()?;
                                    asm.data.push(0x73);
                                    asm.data.push(offset);
                                }
                                Some(Token::Register {
                                    name: RegisterName::H,
                                    ..
                                }) => {
                                    asm.next()?;
                                    asm.data.push(0x74);
                                    asm.data.push(offset);
                                }
                                Some(Token::Register {
                                    name: RegisterName::L,
                                    ..
                                }) => {
                                    asm.next()?;
                                    asm.data.push(0x75);
                                    asm.data.push(offset);
                                }
                                Some(_) => {
                                    asm.data.push(0x36);
                                    asm.data.push(offset);
                                    let (loc, expr) = asm.expr()?;
                                    if let Some(value) = expr.evaluate(&asm.symtab) {
                                        if (value as u32) > (u8::MAX as u32) {
                                            return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                        }
                                        asm.data.push(value as u8);
                                    } else {
                                        asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                        asm.data.push(0);
                                    }
                                }
                            }
                        }
                        Some(_) => {
                            let (loc, expr) = asm.expr()?;
                            asm.expect_symbol(SymbolName::ParenClose)?;
                            asm.expect_symbol(SymbolName::Comma)?;
                            match asm.next()? {
                                None => return asm.end_of_input_err(),
                                Some(Token::Register { name: RegisterName::A, .. }) => {
                                    asm.data.push(0x32);
                                }
                                Some(Token::Register { name: RegisterName::BC, .. }) => {
                                    asm.data.push(0xED);
                                    asm.data.push(0x43);
                                }
                                Some(Token::Register { name: RegisterName::DE, .. }) => {
                                    asm.data.push(0xED);
                                    asm.data.push(0x53);
                                }
                                Some(Token::Register { name: RegisterName::HL, .. }) => {
                                    asm.data.push(0x22);
                                }
                                Some(Token::Register { name: RegisterName::SP, .. }) => {
                                    asm.data.push(0xED);
                                    asm.data.push(0x73);
                                }
                                Some(Token::Register { name: RegisterName::IX, .. }) => {
                                    asm.data.push(0xDD);
                                    asm.data.push(0x22);
                                }
                                Some(Token::Register { name: RegisterName::IY, .. }) => {
                                    asm.data.push(0xFD);
                                    asm.data.push(0x22);
                                }
                                Some(tok) => return asm_err!(tok.loc(),"Unexpected {}, expected registers \"a\", \"bc\", \"de\", \"hl\", \"sp\", \"ix\", or \"iy\"",tok.as_display(&asm.str_interner)),
                            }
                            if let Some(value) = expr.evaluate(&asm.symtab) {
                                if (value as u32) > (u16::MAX as u32) {
                                    return asm_err!(
                                        loc,
                                        "Expression result ({value}) will not fit in a word"
                                    );
                                }
                                asm.data.extend_from_slice(&(value as u16).to_le_bytes());
                            } else {
                                asm.links.push(Link::word(loc, asm.data.len(), expr));
                                asm.data.push(0);
                                asm.data.push(0);
                            }
                        }
                    },

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected a valid \"ld\" destination",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                }
            }

            OperationName::Ldd => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0xA8);
            }

            OperationName::Lddr => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0xB8);
            }

            OperationName::Ldi => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0xA0);
            }

            OperationName::Ldir => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0xB0);
            }

            OperationName::Neg => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0x44);
            }

            OperationName::Nop => {
                asm.next()?;
                asm.data.push(0x00);
            }

            OperationName::Or => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),
                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xB7);
                    }

                    Some(Token::Register {
                        name: RegisterName::B,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xB0);
                    }

                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xB1);
                    }

                    Some(Token::Register {
                        name: RegisterName::D,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xB2);
                    }

                    Some(Token::Register {
                        name: RegisterName::E,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xB3);
                    }

                    Some(Token::Register {
                        name: RegisterName::H,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xB4);
                    }

                    Some(Token::Register {
                        name: RegisterName::L,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xB5);
                    }

                    Some(Token::Register {
                        name: RegisterName::IXH,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xDD);
                        asm.data.push(0xB4);
                    }

                    Some(Token::Register {
                        name: RegisterName::IXL,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xDD);
                        asm.data.push(0xB5);
                    }

                    Some(Token::Register {
                        name: RegisterName::IYH,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xFD);
                        asm.data.push(0xB4);
                    }

                    Some(Token::Register {
                        name: RegisterName::IYL,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xFD);
                        asm.data.push(0xB5);
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.next()?;
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Register {
                                name: RegisterName::HL,
                                ..
                            }) => {
                                asm.data.push(0xB6);
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(Token::Register {
                                name: RegisterName::IX,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xDD);
                                asm.data.push(0xB6);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(Token::Register {
                                name: RegisterName::IY,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xFD);
                                asm.data.push(0xB6);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }
                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected registers \"hl\", \"ix\", or \"iy\"",
                                    tok.as_display(&asm.str_interner)
                                )
                            }
                        }
                    }

                    Some(_) => {
                        asm.data.push(0xF6);
                        let (loc, expr) = asm.expr()?;
                        if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u8::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a byte"
                                );
                            }
                            asm.data.push(value as u8);
                        } else {
                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                            asm.data.push(0);
                        }
                    }
                }
            }

            OperationName::Otdr => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0xBB);
            }

            OperationName::Otir => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0xB3);
            }

            OperationName::Out => {
                asm.next()?;
                asm.expect_symbol(SymbolName::ParenOpen)?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),

                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.next()?;
                        asm.expect_symbol(SymbolName::ParenClose)?;
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.data.push(0xED);
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Register {
                                name: RegisterName::A,
                                ..
                            }) => {
                                asm.data.push(0x79);
                            }
                            Some(Token::Register {
                                name: RegisterName::B,
                                ..
                            }) => {
                                asm.data.push(0x41);
                            }
                            Some(Token::Register {
                                name: RegisterName::C,
                                ..
                            }) => {
                                asm.data.push(0x49);
                            }
                            Some(Token::Register {
                                name: RegisterName::D,
                                ..
                            }) => {
                                asm.data.push(0x51);
                            }
                            Some(Token::Register {
                                name: RegisterName::E,
                                ..
                            }) => {
                                asm.data.push(0x59);
                            }
                            Some(Token::Register {
                                name: RegisterName::H,
                                ..
                            }) => {
                                asm.data.push(0x61);
                            }
                            Some(Token::Register {
                                name: RegisterName::L,
                                ..
                            }) => {
                                asm.data.push(0x69);
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

                    Some(_) => {
                        asm.data.push(0xD3);
                        let (loc, expr) = asm.expr()?;
                        if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u8::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a byte"
                                );
                            }
                            asm.data.push(value as u8);
                        } else {
                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                            asm.data.push(0);
                        }
                        asm.expect_symbol(SymbolName::ParenClose)?;
                        asm.expect_symbol(SymbolName::Comma)?;
                        asm.expect_register(RegisterName::A)?;
                    }
                }
            }

            OperationName::Outd => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0xAB);
            }

            OperationName::Outi => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0xA3);
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
                    Some(Token::Register { name: RegisterName::IX, .. }) => {
                        asm.data.push(0xDD);
                        asm.data.push(0xE1);
                    }
                    Some(Token::Register { name: RegisterName::IY, .. }) => {
                        asm.data.push(0xFD);
                        asm.data.push(0xE1);
                    }
                    Some(tok) => return asm_err!(tok.loc(),"Unexpected {}, expected register \"bc\", \"de\", \"hl\", \"af\", \"ix\", or \"iy\"",tok.as_display(&asm.str_interner)),
                }
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
                    Some(Token::Register { name: RegisterName::IX, .. }) => {
                        asm.data.push(0xDD);
                        asm.data.push(0xE5);
                    }
                    Some(Token::Register { name: RegisterName::IY, .. }) => {
                        asm.data.push(0xFD);
                        asm.data.push(0xE5);
                    }
                    Some(tok) => return asm_err!(tok.loc(),"Unexpected {}, expected register \"bc\", \"de\", \"hl\", \"af\", \"ix\", or \"iy\"",tok.as_display(&asm.str_interner)),
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
                                match asm.next()? {
                                    None => return asm.end_of_input_err(),
                                    Some(Token::Register { name: RegisterName::HL, .. }) => {
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

                                    Some(Token::Register { name: RegisterName::IX, .. }) => {
                                        asm.expect_symbol(SymbolName::Plus)?;
                                        asm.data.push(0xDD);
                                        asm.data.push(0xCB);
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
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

                                    Some(Token::Register { name: RegisterName::IY, .. }) => {
                                        asm.expect_symbol(SymbolName::Plus)?;
                                        asm.data.push(0xFD);
                                        asm.data.push(0xCB);
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
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

                                    Some(tok) => return asm_err!(tok.loc(),"Unexpected {}, expected register \"hl\", \"ix\", or \"iy\"",tok.as_display(&asm.str_interner)),
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
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

            OperationName::Ret => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err()?,
                    Some(Token::Flag {
                        name: FlagName::NotZero,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xC0);
                    }
                    Some(Token::Flag {
                        name: FlagName::Zero,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xC8);
                    }
                    Some(Token::Flag {
                        name: FlagName::NotCarry,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xD0);
                    }
                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xD8);
                    }
                    Some(Token::Flag {
                        name: FlagName::ParityOdd,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xE0);
                    }
                    Some(Token::Flag {
                        name: FlagName::ParityEven,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xE8);
                    }
                    Some(Token::Flag {
                        name: FlagName::Positive,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xF0);
                    }
                    Some(Token::Flag {
                        name: FlagName::Negative,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xF8);
                    }
                    Some(_) => {
                        asm.data.push(0xC9);
                    }
                }
            }

            OperationName::Reti => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0x4D);
            }

            OperationName::Retn => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0x45);
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
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Register {
                                name: RegisterName::HL,
                                ..
                            }) => {
                                asm.data.push(0xCB);
                                asm.data.push(0x16);
                            }
                            Some(Token::Register {
                                name: RegisterName::IX,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xDD);
                                asm.data.push(0xCB);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.data.push(0x16);
                            }
                            Some(Token::Register {
                                name: RegisterName::IY,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xFD);
                                asm.data.push(0xCB);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.data.push(0x16);
                            }
                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected register \"hl\", \"ix\", or \"iy\"",
                                    tok.as_display(&asm.str_interner)
                                )
                            }
                        }
                        asm.expect_symbol(SymbolName::ParenClose)?;
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

            OperationName::Rla => {
                asm.next()?;
                asm.data.push(0x17);
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
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Register {
                                name: RegisterName::HL,
                                ..
                            }) => {
                                asm.data.push(0xCB);
                                asm.data.push(0x06);
                            }
                            Some(Token::Register {
                                name: RegisterName::IX,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xDD);
                                asm.data.push(0xCB);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.data.push(0x06);
                            }
                            Some(Token::Register {
                                name: RegisterName::IY,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xFD);
                                asm.data.push(0xCB);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.data.push(0x06);
                            }
                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected register \"hl\", \"ix\", or \"iy\"",
                                    tok.as_display(&asm.str_interner)
                                )
                            }
                        }
                        asm.expect_symbol(SymbolName::ParenClose)?;
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

            OperationName::Rlca => {
                asm.next()?;
                asm.data.push(0x07);
            }

            OperationName::Rld => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0x6F);
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
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Register {
                                name: RegisterName::HL,
                                ..
                            }) => {
                                asm.data.push(0xCB);
                                asm.data.push(0x1E);
                            }
                            Some(Token::Register {
                                name: RegisterName::IX,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xDD);
                                asm.data.push(0xCB);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.data.push(0x1E);
                            }
                            Some(Token::Register {
                                name: RegisterName::IY,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xFD);
                                asm.data.push(0xCB);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.data.push(0x1E);
                            }
                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected register \"hl\", \"ix\", or \"iy\"",
                                    tok.as_display(&asm.str_interner)
                                )
                            }
                        }
                        asm.expect_symbol(SymbolName::ParenClose)?;
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

            OperationName::Rra => {
                asm.next()?;
                asm.data.push(0x1F);
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
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Register {
                                name: RegisterName::HL,
                                ..
                            }) => {
                                asm.data.push(0xCB);
                                asm.data.push(0x0E);
                            }
                            Some(Token::Register {
                                name: RegisterName::IX,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xDD);
                                asm.data.push(0xCB);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.data.push(0x0E);
                            }
                            Some(Token::Register {
                                name: RegisterName::IY,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xFD);
                                asm.data.push(0xCB);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.data.push(0x0E);
                            }
                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected register \"hl\", \"ix\", or \"iy\"",
                                    tok.as_display(&asm.str_interner)
                                )
                            }
                        }
                        asm.expect_symbol(SymbolName::ParenClose)?;
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

            OperationName::Rrca => {
                asm.next()?;
                asm.data.push(0x0F);
            }

            OperationName::Rrd => {
                asm.next()?;
                asm.data.push(0xED);
                asm.data.push(0x67);
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

            OperationName::Sbc => {
                asm.next()?;
                match asm.next()? {
                    None => return asm.end_of_input_err(),
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
                                asm.data.push(0x9F);
                            }

                            Some(Token::Register {
                                name: RegisterName::B,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x98);
                            }

                            Some(Token::Register {
                                name: RegisterName::C,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x99);
                            }

                            Some(Token::Register {
                                name: RegisterName::D,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x9A);
                            }

                            Some(Token::Register {
                                name: RegisterName::E,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x9B);
                            }

                            Some(Token::Register {
                                name: RegisterName::H,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x9C);
                            }

                            Some(Token::Register {
                                name: RegisterName::L,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0x9D);
                            }

                            Some(Token::Register {
                                name: RegisterName::IXH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x9C);
                            }

                            Some(Token::Register {
                                name: RegisterName::IXL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xDD);
                                asm.data.push(0x9D);
                            }

                            Some(Token::Register {
                                name: RegisterName::IYH,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x9C);
                            }

                            Some(Token::Register {
                                name: RegisterName::IYL,
                                ..
                            }) => {
                                asm.next()?;
                                asm.data.push(0xFD);
                                asm.data.push(0x9D);
                            }

                            Some(Token::Symbol {
                                name: SymbolName::ParenOpen,
                                ..
                            }) => {
                                asm.next()?;
                                match asm.next()? {
                                    None => return asm.end_of_input_err(),
                                    Some(Token::Register {
                                        name: RegisterName::HL,
                                        ..
                                    }) => {
                                        asm.data.push(0x9E);
                                        asm.expect_symbol(SymbolName::ParenClose)?;
                                    }

                                    Some(Token::Register {
                                        name: RegisterName::IX,
                                        ..
                                    }) => {
                                        asm.expect_symbol(SymbolName::Plus)?;
                                        asm.data.push(0xDD);
                                        asm.data.push(0x9E);
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
                                        asm.expect_symbol(SymbolName::ParenClose)?;
                                    }

                                    Some(Token::Register {
                                        name: RegisterName::IY,
                                        ..
                                    }) => {
                                        asm.expect_symbol(SymbolName::Plus)?;
                                        asm.data.push(0xFD);
                                        asm.data.push(0x9E);
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
                                        asm.expect_symbol(SymbolName::ParenClose)?;
                                    }

                                    Some(_) => {
                                        asm.data.push(0x9E);
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
                                        asm.expect_symbol(SymbolName::ParenClose)?;
                                    }
                                }
                            }

                            Some(_) => {
                                asm.data.push(0xDE);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                            }
                        }
                    }

                    Some(Token::Register {
                        name: RegisterName::HL,
                        ..
                    }) => {
                        asm.expect_symbol(SymbolName::Comma)?;
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Register {
                                name: RegisterName::BC,
                                ..
                            }) => {
                                asm.data.push(0xED);
                                asm.data.push(0x42);
                            }

                            Some(Token::Register {
                                name: RegisterName::DE,
                                ..
                            }) => {
                                asm.data.push(0xED);
                                asm.data.push(0x52);
                            }

                            Some(Token::Register {
                                name: RegisterName::HL,
                                ..
                            }) => {
                                asm.data.push(0xED);
                                asm.data.push(0x62);
                            }

                            Some(Token::Register {
                                name: RegisterName::SP,
                                ..
                            }) => {
                                asm.data.push(0xED);
                                asm.data.push(0x72);
                            }

                            Some(tok) => {
                                return asm_err!(
                                tok.loc(),
                                "Unexpected {}, expected register \"bc\", \"de\", \"hl\" or \"sp\"",
                                tok.as_display(&asm.str_interner)
                            )
                            }
                        }
                    }

                    Some(tok) => {
                        return asm_err!(
                            tok.loc(),
                            "Unexpected {}, expected register \"a\" or \"hl\"",
                            tok.as_display(&asm.str_interner)
                        )
                    }
                }
            }

            OperationName::Scf => {
                asm.next()?;
                asm.data.push(0x37);
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
                                match asm.next()? {
                                    None => return asm.end_of_input_err(),
                                    Some(Token::Register { name: RegisterName::HL, .. }) => {
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

                                    Some(Token::Register { name: RegisterName::IX, .. }) => {
                                        asm.expect_symbol(SymbolName::Plus)?;
                                        asm.data.push(0xDD);
                                        asm.data.push(0xCB);
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
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

                                    Some(Token::Register { name: RegisterName::IY, .. }) => {
                                        asm.expect_symbol(SymbolName::Plus)?;
                                        asm.data.push(0xFD);
                                        asm.data.push(0xCB);
                                        let (loc, expr) = asm.expr()?;
                                        if let Some(value) = expr.evaluate(&asm.symtab) {
                                            if (value as u32) > (u8::MAX as u32) {
                                                return asm_err!(loc,"Expression result ({value}) will not fit in a byte");
                                            }
                                            asm.data.push(value as u8);
                                        } else {
                                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                            asm.data.push(0);
                                        }
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

                                    Some(tok) => return asm_err!(tok.loc(),"Unexpected {}, expected register \"hl\", \"ix\", or \"iy\"",tok.as_display(&asm.str_interner)),
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
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
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Register {
                                name: RegisterName::HL,
                                ..
                            }) => {
                                asm.data.push(0xCB);
                                asm.data.push(0x26);
                            }
                            Some(Token::Register {
                                name: RegisterName::IX,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xDD);
                                asm.data.push(0xCB);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.data.push(0x26);
                            }
                            Some(Token::Register {
                                name: RegisterName::IY,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xFD);
                                asm.data.push(0xCB);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.data.push(0x26);
                            }
                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected register \"hl\", \"ix\", or \"iy\"",
                                    tok.as_display(&asm.str_interner)
                                )
                            }
                        }
                        asm.expect_symbol(SymbolName::ParenClose)?;
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

            OperationName::Sll => {
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
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Register {
                                name: RegisterName::HL,
                                ..
                            }) => {
                                asm.data.push(0xCB);
                                asm.data.push(0x36);
                            }
                            Some(Token::Register {
                                name: RegisterName::IX,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xDD);
                                asm.data.push(0xCB);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.data.push(0x36);
                            }
                            Some(Token::Register {
                                name: RegisterName::IY,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xFD);
                                asm.data.push(0xCB);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.data.push(0x36);
                            }
                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected register \"hl\", \"ix\", or \"iy\"",
                                    tok.as_display(&asm.str_interner)
                                )
                            }
                        }
                        asm.expect_symbol(SymbolName::ParenClose)?;
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
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Register {
                                name: RegisterName::HL,
                                ..
                            }) => {
                                asm.data.push(0xCB);
                                asm.data.push(0x2E);
                            }
                            Some(Token::Register {
                                name: RegisterName::IX,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xDD);
                                asm.data.push(0xCB);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.data.push(0x2E);
                            }
                            Some(Token::Register {
                                name: RegisterName::IY,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xFD);
                                asm.data.push(0xCB);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.data.push(0x2E);
                            }
                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected register \"hl\", \"ix\", or \"iy\"",
                                    tok.as_display(&asm.str_interner)
                                )
                            }
                        }
                        asm.expect_symbol(SymbolName::ParenClose)?;
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
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Register {
                                name: RegisterName::HL,
                                ..
                            }) => {
                                asm.data.push(0xCB);
                                asm.data.push(0x3E);
                            }
                            Some(Token::Register {
                                name: RegisterName::IX,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xDD);
                                asm.data.push(0xCB);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.data.push(0x3E);
                            }
                            Some(Token::Register {
                                name: RegisterName::IY,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xFD);
                                asm.data.push(0xCB);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.data.push(0x3E);
                            }
                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected register \"hl\", \"ix\", or \"iy\"",
                                    tok.as_display(&asm.str_interner)
                                )
                            }
                        }
                        asm.expect_symbol(SymbolName::ParenClose)?;
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

            OperationName::Sub => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),
                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0x97);
                    }

                    Some(Token::Register {
                        name: RegisterName::B,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0x90);
                    }

                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0x91);
                    }

                    Some(Token::Register {
                        name: RegisterName::D,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0x92);
                    }

                    Some(Token::Register {
                        name: RegisterName::E,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0x93);
                    }

                    Some(Token::Register {
                        name: RegisterName::H,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0x94);
                    }

                    Some(Token::Register {
                        name: RegisterName::L,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0x95);
                    }

                    Some(Token::Register {
                        name: RegisterName::IXH,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xDD);
                        asm.data.push(0x94);
                    }

                    Some(Token::Register {
                        name: RegisterName::IXL,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xDD);
                        asm.data.push(0x95);
                    }

                    Some(Token::Register {
                        name: RegisterName::IYH,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xFD);
                        asm.data.push(0x94);
                    }

                    Some(Token::Register {
                        name: RegisterName::IYL,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xFD);
                        asm.data.push(0x95);
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.next()?;
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Register {
                                name: RegisterName::HL,
                                ..
                            }) => {
                                asm.data.push(0x96);
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(Token::Register {
                                name: RegisterName::IX,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xDD);
                                asm.data.push(0x96);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(Token::Register {
                                name: RegisterName::IY,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xFD);
                                asm.data.push(0x96);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }
                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected register \"hl\", \"ix\", or \"iy\"",
                                    tok.as_display(&asm.str_interner)
                                )
                            }
                        }
                    }
                    Some(_) => {
                        asm.data.push(0xD6);
                        let (loc, expr) = asm.expr()?;
                        if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u8::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a byte"
                                );
                            }
                            asm.data.push(value as u8);
                        } else {
                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                            asm.data.push(0);
                        }
                    }
                }
            }

            OperationName::Xor => {
                asm.next()?;
                match asm.peek()? {
                    None => return asm.end_of_input_err(),
                    Some(Token::Register {
                        name: RegisterName::A,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xAF);
                    }

                    Some(Token::Register {
                        name: RegisterName::B,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xA8);
                    }

                    Some(Token::Register {
                        name: RegisterName::C,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xA9);
                    }

                    Some(Token::Register {
                        name: RegisterName::D,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xAA);
                    }

                    Some(Token::Register {
                        name: RegisterName::E,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xAB);
                    }

                    Some(Token::Register {
                        name: RegisterName::H,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xAC);
                    }

                    Some(Token::Register {
                        name: RegisterName::L,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xAD);
                    }

                    Some(Token::Register {
                        name: RegisterName::IXH,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xDD);
                        asm.data.push(0xAC);
                    }

                    Some(Token::Register {
                        name: RegisterName::IXL,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xDD);
                        asm.data.push(0xAD);
                    }

                    Some(Token::Register {
                        name: RegisterName::IYH,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xFD);
                        asm.data.push(0xAC);
                    }

                    Some(Token::Register {
                        name: RegisterName::IYL,
                        ..
                    }) => {
                        asm.next()?;
                        asm.data.push(0xFD);
                        asm.data.push(0xAD);
                    }

                    Some(Token::Symbol {
                        name: SymbolName::ParenOpen,
                        ..
                    }) => {
                        asm.next()?;
                        match asm.next()? {
                            None => return asm.end_of_input_err(),
                            Some(Token::Register {
                                name: RegisterName::HL,
                                ..
                            }) => {
                                asm.data.push(0xAE);
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(Token::Register {
                                name: RegisterName::IX,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xDD);
                                asm.data.push(0xAE);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }

                            Some(Token::Register {
                                name: RegisterName::IY,
                                ..
                            }) => {
                                asm.expect_symbol(SymbolName::Plus)?;
                                asm.data.push(0xFD);
                                asm.data.push(0xAE);
                                let (loc, expr) = asm.expr()?;
                                if let Some(value) = expr.evaluate(&asm.symtab) {
                                    if (value as u32) > (u8::MAX as u32) {
                                        return asm_err!(
                                            loc,
                                            "Expression result ({value}) will not fit in a byte"
                                        );
                                    }
                                    asm.data.push(value as u8);
                                } else {
                                    asm.links.push(Link::byte(loc, asm.data.len(), expr));
                                    asm.data.push(0);
                                }
                                asm.expect_symbol(SymbolName::ParenClose)?;
                            }
                            Some(tok) => {
                                return asm_err!(
                                    tok.loc(),
                                    "Unexpected {}, expected registers \"hl\", \"ix\", or \"iy\"",
                                    tok.as_display(&asm.str_interner)
                                )
                            }
                        }
                    }

                    Some(_) => {
                        asm.data.push(0xEE);
                        let (loc, expr) = asm.expr()?;
                        if let Some(value) = expr.evaluate(&asm.symtab) {
                            if (value as u32) > (u8::MAX as u32) {
                                return asm_err!(
                                    loc,
                                    "Expression result ({value}) will not fit in a byte"
                                );
                            }
                            asm.data.push(value as u8);
                        } else {
                            asm.links.push(Link::byte(loc, asm.data.len(), expr));
                            asm.data.push(0);
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

use std::{
    io::{self, Cursor},
    path::{Path, PathBuf},
};

use super::*;

use crate::{assembler::Assembler, fileman::FileSystem};
use fxhash::FxHashMap;

struct StringFileSystem {
    files: FxHashMap<PathBuf, String>,
}

impl StringFileSystem {
    #[inline]
    fn new<P: AsRef<Path>>(files: &[(P, &str)]) -> Self {
        let mut map = FxHashMap::default();
        for (path, s) in files {
            map.insert(path.as_ref().to_path_buf(), s.to_string());
        }
        Self { files: map }
    }
}

impl FileSystem for StringFileSystem {
    type Reader = Cursor<String>;
    type Writer = Cursor<Vec<u8>>;

    #[inline]
    fn is_dir(&self, _: &Path) -> io::Result<bool> {
        Ok(true)
    }

    #[inline]
    fn is_file(&self, path: &Path) -> io::Result<bool> {
        Ok(self.files.contains_key(path))
    }

    #[inline]
    fn open_read(&self, path: &Path) -> io::Result<Self::Reader> {
        Ok(Cursor::new(self.files.get(path).unwrap().clone()))
    }

    #[inline]
    fn open_write(&self, path: &Path) -> io::Result<Self::Writer> {
        Ok(Cursor::new(
            self.files.get(path).unwrap().clone().into_bytes(),
        ))
    }
}

fn assembler<P: AsRef<Path>>(
    files: &[(P, &str)],
) -> Assembler<StringFileSystem, Cursor<String>, Mos6502Tokens, Mos6502> {
    Assembler::new(StringFileSystem::new(files), Mos6502)
}

#[test]
fn adc() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            adc #$42
            adc $42
            adc $42, x
            adc $cafe
            adc $cafe, x
            adc $cafe, y
            adc ($42, x)
            adc ($42), y
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x69, 0x42,
        0x65, 0x42,
        0x75, 0x42,
        0x6D, 0xfe, 0xca,
        0x7D, 0xfe, 0xca,
        0x79, 0xfe, 0xca,
        0x61, 0x42,
        0x71, 0x42,
        19, 0x00
    ], data);
}

#[test]
fn and() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            and #$42
            and $42
            and $42, x
            and $cafe
            and $cafe, x
            and $cafe, y
            and ($42, x)
            and ($42), y
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x29, 0x42,
        0x25, 0x42,
        0x35, 0x42,
        0x2D, 0xfe, 0xca,
        0x3D, 0xfe, 0xca,
        0x39, 0xfe, 0xca,
        0x21, 0x42,
        0x31, 0x42,
        19, 0x00
    ], data);
}

#[test]
fn asl() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            asl a
            asl $42
            asl $42, x
            asl $cafe
            asl $cafe, x
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x0A,
        0x06, 0x42,
        0x16, 0x42,
        0x0E, 0xfe, 0xca,
        0x1E, 0xfe, 0xca,
        11, 0x00
    ], data);
}

#[test]
fn bcc() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @org 100
            bar: nop
            @org 0
            foo: bcc foo
            bcc bar
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xEA,
        0x90, -2i8 as u8,
        0x90, 96i8 as u8,
        4, 0x00
    ], data);
}

#[test]
fn bcs() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @org 100
            bar: nop
            @org 0
            foo: bcs foo
            bcs bar
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xEA,
        0xB0, -2i8 as u8,
        0xB0, 96i8 as u8,
        4, 0x00
    ], data);
}

#[test]
fn beq() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @org 100
            bar: nop
            @org 0
            foo: beq foo
            beq bar
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xEA,
        0xF0, -2i8 as u8,
        0xF0, 96i8 as u8,
        4, 0x00
    ], data);
}

#[test]
fn bit() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            bit $42
            bit $cafe
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x24, 0x42,
        0x2C, 0xfe, 0xca,
        5, 0x00
    ], data);
}

#[test]
fn bmi() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @org 100
            bar: nop
            @org 0
            foo: bmi foo
            bmi bar
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xEA,
        0x30, -2i8 as u8,
        0x30, 96i8 as u8,
        4, 0x00
    ], data);
}

#[test]
fn bne() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @org 100
            bar: nop
            @org 0
            foo: bne foo
            bne bar
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xEA,
        0xD0, -2i8 as u8,
        0xD0, 96i8 as u8,
        4, 0x00
    ], data);
}

#[test]
fn bpl() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @org 100
            bar: nop
            @org 0
            foo: bpl foo
            bpl bar
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xEA,
        0x10, -2i8 as u8,
        0x10, 96i8 as u8,
        4, 0x00
    ], data);
}

#[test]
fn brk() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            brk
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x00,
        1, 0x00
    ], data);
}

#[test]
fn bvc() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @org 100
            bar: nop
            @org 0
            foo: bvc foo
            bvc bar
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xEA,
        0x50, -2i8 as u8,
        0x50, 96i8 as u8,
        4, 0x00
    ], data);
}

#[test]
fn bvs() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @org 100
            bar: nop
            @org 0
            foo: bvs foo
            bvs bar
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xEA,
        0x70, -2i8 as u8,
        0x70, 96i8 as u8,
        4, 0x00
    ], data);
}

#[test]
fn clc() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            clc
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x18,
        1, 0x00
    ], data);
}

#[test]
fn cld() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            cld
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xD8,
        1, 0x00
    ], data);
}

#[test]
fn cli() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            cli
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x58,
        1, 0x00
    ], data);
}

#[test]
fn clv() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            clv
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xB8,
        1, 0x00
    ], data);
}

#[test]
fn cmp() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            cmp #$42
            cmp $42
            cmp $42, x
            cmp $cafe
            cmp $cafe, x
            cmp $cafe, y
            cmp ($42, x)
            cmp ($42), y
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xC9, 0x42,
        0xC5, 0x42,
        0xD5, 0x42,
        0xCD, 0xfe, 0xca,
        0xDD, 0xfe, 0xca,
        0xD9, 0xfe, 0xca,
        0xC1, 0x42,
        0xD1, 0x42,
        19, 0x00
    ], data);
}

#[test]
fn cpx() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            cpx #$42
            cpx $42
            cpx $cafe
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xE0, 0x42,
        0xE4, 0x42,
        0xEC, 0xfe, 0xca,
        7, 0x00
    ], data);
}

#[test]
fn cpy() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            cpy #$42
            cpy $42
            cpy $cafe
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xC0, 0x42,
        0xC4, 0x42,
        0xCC, 0xfe, 0xca,
        7, 0x00
    ], data);
}

#[test]
fn dec() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            dec $42
            dec $42, x
            dec $cafe
            dec $cafe, x
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xC6, 0x42,
        0xD6, 0x42,
        0xCE, 0xfe, 0xca,
        0xDE, 0xfe, 0xca,
        10, 0x00
    ], data);
}

#[test]
fn dex() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            dex
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xCA,
        1, 0x00
    ], data);
}

#[test]
fn dey() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            dey
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x88,
        1, 0x00
    ], data);
}

#[test]
fn eor() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            eor #$42
            eor $42
            eor $42, x
            eor $cafe
            eor $cafe, x
            eor $cafe, y
            eor ($42, x)
            eor ($42), y
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x49, 0x42,
        0x45, 0x42,
        0x55, 0x42,
        0x4D, 0xfe, 0xca,
        0x5D, 0xfe, 0xca,
        0x59, 0xfe, 0xca,
        0x41, 0x42,
        0x51, 0x42,
        19, 0x00
    ], data);
}

#[test]
fn inc() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            inc $42
            inc $42, x
            inc $cafe
            inc $cafe, x
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xE6, 0x42,
        0xF6, 0x42,
        0xEE, 0xfe, 0xca,
        0xFE, 0xfe, 0xca,
        10, 0x00
    ], data);
}

#[test]
fn inx() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            inx
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xE8,
        1, 0x00
    ], data);
}

#[test]
fn iny() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            iny
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xC8,
        1, 0x00
    ], data);
}

#[test]
fn jmp() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            jmp $cafe
            jmp ($cafe)
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x4C, 0xfe, 0xca,
        0x6C, 0xfe, 0xca,
        6, 0x00
    ], data);
}

#[test]
fn jsr() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            jsr $cafe
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x20, 0xfe, 0xca,
        3, 0x00
    ], data);
}

#[test]
fn lda() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            lda #$42
            lda $42
            lda $42, x
            lda $cafe
            lda $cafe, x
            lda $cafe, y
            lda ($42, x)
            lda ($42), y
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xA9, 0x42,
        0xA5, 0x42,
        0xB5, 0x42,
        0xAD, 0xfe, 0xca,
        0xBD, 0xfe, 0xca,
        0xB9, 0xfe, 0xca,
        0xA1, 0x42,
        0xB1, 0x42,
        19, 0x00
    ], data);
}

#[test]
fn ldx() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ldx #$42
            ldx $42
            ldx $42, y
            ldx $cafe
            ldx $cafe, y
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xA2, 0x42,
        0xA6, 0x42,
        0xB6, 0x42,
        0xAE, 0xfe, 0xca,
        0xBE, 0xfe, 0xca,
        12, 0x00
    ], data);
}

#[test]
fn ldy() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ldy #$42
            ldy $42
            ldy $42, x
            ldy $cafe
            ldy $cafe, x
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xA0, 0x42,
        0xA4, 0x42,
        0xB4, 0x42,
        0xAC, 0xfe, 0xca,
        0xBC, 0xfe, 0xca,
        12, 0x00
    ], data);
}

#[test]
fn lsr() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            lsr a
            lsr $42
            lsr $42, x
            lsr $cafe
            lsr $cafe, x
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x4A,
        0x46, 0x42,
        0x56, 0x42,
        0x4E, 0xfe, 0xca,
        0x5E, 0xfe, 0xca,
        11, 0x00
    ], data);
}

#[test]
fn ora() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ora #$42
            ora $42
            ora $42, x
            ora $cafe
            ora $cafe, x
            ora $cafe, y
            ora ($42, x)
            ora ($42), y
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x09, 0x42,
        0x05, 0x42,
        0x15, 0x42,
        0x0D, 0xfe, 0xca,
        0x1D, 0xfe, 0xca,
        0x19, 0xfe, 0xca,
        0x01, 0x42,
        0x11, 0x42,
        19, 0x00
    ], data);
}

#[test]
fn pha() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            pha
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x48,
        1, 0x00
    ], data);
}

#[test]
fn php() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            php
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x08,
        1, 0x00
    ], data);
}

#[test]
fn pla() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            pla
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x68,
        1, 0x00
    ], data);
}

#[test]
fn plp() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            plp
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x28,
        1, 0x00
    ], data);
}

#[test]
fn rol() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            rol a
            rol $42
            rol $42, x
            rol $cafe
            rol $cafe, x
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x2A,
        0x26, 0x42,
        0x36, 0x42,
        0x2E, 0xfe, 0xca,
        0x3E, 0xfe, 0xca,
        11, 0x00
    ], data);
}

#[test]
fn ror() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ror a
            ror $42
            ror $42, x
            ror $cafe
            ror $cafe, x
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x6A,
        0x66, 0x42,
        0x76, 0x42,
        0x6E, 0xfe, 0xca,
        0x7E, 0xfe, 0xca,
        11, 0x00
    ], data);
}

#[test]
fn rti() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            rti
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x40,
        1, 0x00
    ], data);
}

#[test]
fn rts() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            rts
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x60,
        1, 0x00
    ], data);
}

#[test]
fn sbc() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            sbc #$42
            sbc $42
            sbc $42, x
            sbc $cafe
            sbc $cafe, x
            sbc $cafe, y
            sbc ($42, x)
            sbc ($42), y
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xE9, 0x42,
        0xE5, 0x42,
        0xF5, 0x42,
        0xED, 0xfe, 0xca,
        0xFD, 0xfe, 0xca,
        0xF9, 0xfe, 0xca,
        0xE1, 0x42,
        0xF1, 0x42,
        19, 0x00
    ], data);
}

#[test]
fn sec() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            sec
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x38,
        1, 0x00
    ], data);
}

#[test]
fn sed() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            sed
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xF8,
        1, 0x00
    ], data);
}

#[test]
fn sei() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            sei
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x78,
        1, 0x00
    ], data);
}

#[test]
fn sta() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            sta $42
            sta $42, x
            sta $cafe
            sta $cafe, x
            sta $cafe, y
            sta ($42, x)
            sta ($42), y
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x85, 0x42,
        0x95, 0x42,
        0x8D, 0xfe, 0xca,
        0x9D, 0xfe, 0xca,
        0x99, 0xfe, 0xca,
        0x81, 0x42,
        0x91, 0x42,
        17, 0x00
    ], data);
}

#[test]
fn stx() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            stx $42
            stx $42, y
            stx $cafe
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x86, 0x42,
        0x96, 0x42,
        0x8E, 0xfe, 0xca,
        7, 0x00
    ], data);
}

#[test]
fn sty() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            sty $42
            sty $42, x
            sty $cafe
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x84, 0x42,
        0x94, 0x42,
        0x8C, 0xfe, 0xca,
        7, 0x00
    ], data);
}

#[test]
fn tax() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            tax
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xAA,
        1, 0x00
    ], data);
}

#[test]
fn tay() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            tay
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xA8,
        1, 0x00
    ], data);
}

#[test]
fn tsx() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            tsx
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0xBA,
        1, 0x00
    ], data);
}

#[test]
fn txa() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            txa
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x8A,
        1, 0x00
    ], data);
}

#[test]
fn txs() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            txs
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x9A,
        1, 0x00
    ], data);
}

#[test]
fn tya() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            tya
            @dw @here
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();

    #[rustfmt::skip]
    assert_eq!(vec![
        0x98,
        1, 0x00
    ], data);
}

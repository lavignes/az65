use std::{
    io::{self, Cursor},
    path::{Path, PathBuf},
};

use fxhash::FxHashMap;

use super::*;
use crate::{assembler::Assembler, fileman::FileSystem};

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
) -> Assembler<StringFileSystem, Cursor<String>, Sm83Tokens, Sm83> {
    Assembler::new(StringFileSystem::new(files), Sm83)
}

#[test]
fn nop() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            nop
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
fn ld_bc() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ld bc, $1234
            ld (bc), a
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
        0x01, 0x34, 0x12,
        0x02,
        4, 0x00
    ], data);
}

#[test]
fn ld_de() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ld de, $1234
            ld (de), a
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
        0x11, 0x34, 0x12,
        0x12,
        4, 0x00
    ], data);
}

#[test]
fn ld_hl() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ld hl, $1234
            ld hl, sp+4
            ld (hl+), a
            ld (hl-), a
            ld (hl), a
            ld (hl), b
            ld (hl), c
            ld (hl), d
            ld (hl), e
            ld (hl), h
            ld (hl), l
            ld (hl), $42
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
        0x21, 0x34, 0x12,
        0xF8, 0x04,
        0x22,
        0x32,
        0x77,
        0x70,
        0x71,
        0x72,
        0x73,
        0x74,
        0x75,
        0x36, 0x42,
        16, 0x00
    ], data);
}

#[test]
fn ld_sp() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ld sp, $1234
            ld sp, hl
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
        0x31, 0x34, 0x12,
        0xF9,
        4, 0x00
    ], data);
}

#[test]
fn ld_indirect() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ld ($1234), a
            ld ($1234), sp
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
        0xEA, 0x34, 0x12,
        0x08, 0x34, 0x12,
        6, 0x00
    ], data);
}

#[test]
fn ld_c() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ld c, a
            ld c, b
            ld c, c
            ld c, d
            ld c, e
            ld c, h
            ld c, l
            ld c, (hl)
            ld c, $42
            ld (c), a
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
        0x4F,
        0x48,
        0x49,
        0x4A,
        0x4B,
        0x4C,
        0x4D,
        0x4E,
        0x0E, 0x42,
        0xE2,
        11, 0x00
    ], data);
}

#[test]
fn ld_a() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ld a, a
            ld a, b
            ld a, c
            ld a, d
            ld a, e
            ld a, h
            ld a, l
            ld a, (bc)
            ld a, (de)
            ld a, (c)
            ld a, (hl+)
            ld a, (hl-)
            ld a, (hl)
            ld a, ($1234)
            ld a, $42
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
        0x7F,
        0x78,
        0x79,
        0x7A,
        0x7B,
        0x7C,
        0x7D,
        0x0A,
        0x1A,
        0xF2,
        0x2A,
        0x3A,
        0x7E,
        0xFA, 0x34, 0x12,
        0x3E, 0x42,
        18, 0x00
    ], data);
}

#[test]
fn ld_b() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ld b, a
            ld b, b
            ld b, c
            ld b, d
            ld b, e
            ld b, h
            ld b, l
            ld b, (hl)
            ld b, $42
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
        0x47,
        0x40,
        0x41,
        0x42,
        0x43,
        0x44,
        0x45,
        0x46,
        0x06, 0x42,
        10, 0x00
    ], data);
}

#[test]
fn ld_d() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ld d, a  
            ld d, b  
            ld d, c  
            ld d, d  
            ld d, e  
            ld d, h  
            ld d, l  
            ld d, (hl)
            ld d, $42
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
        0x57,
        0x50,
        0x51,
        0x52,
        0x53,
        0x54,
        0x55,
        0x56,
        0x16, 0x42,
        10, 0x00
    ], data);
}

#[test]
fn ld_e() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ld e, a  
            ld e, b  
            ld e, c  
            ld e, d  
            ld e, e  
            ld e, h  
            ld e, l  
            ld e, (hl)
            ld e, $42
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
        0x5F,
        0x58,
        0x59,
        0x5A,
        0x5B,
        0x5C,
        0x5D,
        0x5E,
        0x1E, 0x42,
        10, 0x00
    ], data);
}

#[test]
fn ld_h() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ld h, a  
            ld h, b  
            ld h, c  
            ld h, d  
            ld h, e  
            ld h, h  
            ld h, l  
            ld h, (hl)
            ld h, $42
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
        0x67,
        0x60,
        0x61,
        0x62,
        0x63,
        0x64,
        0x65,
        0x66,
        0x26, 0x42,
        10, 0x00
    ], data);
}

#[test]
fn ld_l() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ld l, a  
            ld l, b  
            ld l, c  
            ld l, d  
            ld l, e  
            ld l, h  
            ld l, l  
            ld l, (hl)
            ld l, $42
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
        0x6F,
        0x68,
        0x69,
        0x6A,
        0x6B,
        0x6C,
        0x6D,
        0x6E,
        0x2E, 0x42,
        10, 0x00
    ], data);
}

#[test]
fn ldh() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ldh ($42), a
            ldh a, ($42)
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
        0xF0, 0x42,
        4, 0x00
    ], data);
}

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
    fn exists(&self, _: &Path) -> bool {
        true
    }

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

#[test]
fn jr() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            test:
                nop
                jr nz, test
                jr nc, test
                jr z, test
                jr c, test
                jr test
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
        0x20, (-3_i8 as u8),
        0x30, (-5_i8 as u8),
        0x28, (-7_i8 as u8),
        0x38, (-9_i8 as u8),
        0x18, (-11_i8 as u8),
        11, 0x00
    ], data);
}

#[test]
fn inc() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            inc bc
            inc de
            inc hl
            inc sp
            inc a
            inc b
            inc c
            inc d
            inc e
            inc h
            inc l
            inc (hl)
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
        0x03,
        0x13,
        0x23,
        0x33,
        0x3C,
        0x04,
        0x0C,
        0x14,
        0x1C,
        0x24,
        0x2C,
        0x34,
        12, 0x00
    ], data);
}

#[test]
fn dec() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            dec bc
            dec de
            dec hl
            dec sp
            dec a
            dec b
            dec c
            dec d
            dec e
            dec h
            dec l
            dec (hl)
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
        0x0B,
        0x1B,
        0x2B,
        0x3B,
        0x3D,
        0x05,
        0x0D,
        0x15,
        0x1D,
        0x25,
        0x2D,
        0x35,
        12, 0x00
    ], data);
}

#[test]
fn rot_a() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            rlca
            rrca
            rla
            rra
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
        0x07,
        0x0F,
        0x17,
        0x1F,
        4, 0x00
    ], data);
}

#[test]
fn misc() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            daa
            scf
            cpl
            ccf
            halt
            di
            ei
            stop
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
        0x27,
        0x37,
        0x2F,
        0x3F,
        0x76,
        0xF3,
        0xFB,
        0x10,
        8, 0x00
    ], data);
}

#[test]
fn add() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            add hl, bc
            add hl, de
            add hl, hl
            add hl, sp
            add sp, $42
            add a, a
            add a, b
            add a, c
            add a, d
            add a, e
            add a, h
            add a, l
            add a, (hl)
            add a, $42
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
        0x09,
        0x19,
        0x29,
        0x39,
        0xE8, 0x42,
        0x87,
        0x80,
        0x81,
        0x82,
        0x83,
        0x84,
        0x85,
        0x86,
        0xC6, 0x42,
        16, 0x00
    ], data);
}

#[test]
fn adc() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            adc a, a
            adc a, b
            adc a, c
            adc a, d
            adc a, e
            adc a, h
            adc a, l
            adc a, (hl)
            adc a, $42
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
        0x8F,
        0x88,
        0x89,
        0x8A,
        0x8B,
        0x8C,
        0x8D,
        0x8E,
        0xCE, 0x42,
        10, 0x00
    ], data);
}

#[test]
fn sub() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            sub a
            sub b
            sub c
            sub d
            sub e
            sub h
            sub l
            sub (hl)
            sub $42
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
        0x97,
        0x90,
        0x91,
        0x92,
        0x93,
        0x94,
        0x95,
        0x96,
        0xD6, 0x42,
        10, 0x00
    ], data);
}

#[test]
fn sbc() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            sbc a, a
            sbc a, b
            sbc a, c
            sbc a, d
            sbc a, e
            sbc a, h
            sbc a, l
            sbc a, (hl)
            sbc a, $42
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
        0x9F,
        0x98,
        0x99,
        0x9A,
        0x9B,
        0x9C,
        0x9D,
        0x9E,
        0xDE, 0x42,
        10, 0x00
    ], data);
}

#[test]
fn and() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            and a
            and b
            and c
            and d
            and e
            and h
            and l
            and (hl)
            and $42
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
        0xA7,
        0xA0,
        0xA1,
        0xA2,
        0xA3,
        0xA4,
        0xA5,
        0xA6,
        0xE6, 0x42,
        10, 0x00
    ], data);
}

#[test]
fn xor() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            xor a
            xor b
            xor c
            xor d
            xor e
            xor h
            xor l
            xor (hl)
            xor $42
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
        0xAF,
        0xA8,
        0xA9,
        0xAA,
        0xAB,
        0xAC,
        0xAD,
        0xAE,
        0xEE, 0x42,
        10, 0x00
    ], data);
}

#[test]
fn or() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            or a
            or b
            or c
            or d
            or e
            or h
            or l
            or (hl)
            or $42
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
        0xB7,
        0xB0,
        0xB1,
        0xB2,
        0xB3,
        0xB4,
        0xB5,
        0xB6,
        0xF6, 0x42,
        10, 0x00
    ], data);
}

#[test]
fn cp() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            cp a
            cp b
            cp c
            cp d
            cp e
            cp h
            cp l
            cp (hl)
            cp $42
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
        0xCF,
        0xC8,
        0xC9,
        0xCA,
        0xCB,
        0xCC,
        0xCD,
        0xCE,
        0xFE, 0x42,
        10, 0x00
    ], data);
}

#[test]
fn ret_reti() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ret nz
            ret nc
            ret z
            ret c
            ret
            reti
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
        0xC0,
        0xD0,
        0xC8,
        0xD8,
        0xC9,
        0xD9,
        6, 0x00
    ], data);
}

#[test]
fn jp() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            jp nz, $1234
            jp nc, $1234
            jp z, $1234
            jp c, $1234
            jp hl
            jp $1234
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
        0xC2, 0x34, 0x12,
        0xD2, 0x34, 0x12,
        0xCA, 0x34, 0x12,
        0xDA, 0x34, 0x12,
        0xE9,
        0xC3, 0x34, 0x12,
        16, 0x00
    ], data);
}

#[test]
fn call() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            call nz, $1234
            call nc, $1234
            call z, $1234
            call c, $1234
            call $1234
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
        0xC4, 0x34, 0x12,
        0xD4, 0x34, 0x12,
        0xCC, 0x34, 0x12,
        0xDC, 0x34, 0x12,
        0xCD, 0x34, 0x12,
        15, 0x00
    ], data);
}

#[test]
fn rst() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            rst $00
            rst $08
            rst $10
            rst $18
            rst $20
            rst $28
            rst $30
            rst $38
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
        0xC7,
        0xCF,
        0xD7,
        0xDF,
        0xE7,
        0xEF,
        0xF7,
        0xFF,
        8, 0x00
    ], data);
}

#[test]
fn push_pop() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            pop bc
            pop de
            pop hl
            pop af
            push bc
            push de
            push hl
            push af
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
        0xC1,
        0xD1,
        0xE1,
        0xF1,
        0xC5,
        0xD5,
        0xE5,
        0xF5,
        8, 0x00
    ], data);
}

#[test]
fn rlc() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            rlc a
            rlc b
            rlc c
            rlc d
            rlc e
            rlc h
            rlc l
            rlc (hl)
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
        0xCB, 0x07,
        0xCB, 0x00,
        0xCB, 0x01,
        0xCB, 0x02,
        0xCB, 0x03,
        0xCB, 0x04,
        0xCB, 0x05,
        0xCB, 0x06,
        16, 0x00
    ], data);
}

#[test]
fn rrc() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            rrc a
            rrc b
            rrc c
            rrc d
            rrc e
            rrc h
            rrc l
            rrc (hl)
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
        0xCB, 0x0F,
        0xCB, 0x08,
        0xCB, 0x09,
        0xCB, 0x0A,
        0xCB, 0x0B,
        0xCB, 0x0C,
        0xCB, 0x0D,
        0xCB, 0x0E,
        16, 0x00
    ], data);
}

#[test]
fn rl() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            rl a
            rl b
            rl c
            rl d
            rl e
            rl h
            rl l
            rl (hl)
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
        0xCB, 0x17,
        0xCB, 0x10,
        0xCB, 0x11,
        0xCB, 0x12,
        0xCB, 0x13,
        0xCB, 0x14,
        0xCB, 0x15,
        0xCB, 0x16,
        16, 0x00
    ], data);
}

#[test]
fn rr() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            rr a
            rr b
            rr c
            rr d
            rr e
            rr h
            rr l
            rr (hl)
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
        0xCB, 0x1F, 
        0xCB, 0x18, 
        0xCB, 0x19, 
        0xCB, 0x1A, 
        0xCB, 0x1B, 
        0xCB, 0x1C, 
        0xCB, 0x1D, 
        0xCB, 0x1E, 
        16, 0x00
    ], data);
}

#[test]
fn sla() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            sla a
            sla b
            sla c
            sla d
            sla e
            sla h
            sla l
            sla (hl)
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
        0xCB, 0x27,
        0xCB, 0x20,
        0xCB, 0x21,
        0xCB, 0x22,
        0xCB, 0x23,
        0xCB, 0x24,
        0xCB, 0x25,
        0xCB, 0x26,
        16, 0x00  
    ], data);
}

#[test]
fn sra() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"       
            sra a  
            sra b  
            sra c  
            sra d  
            sra e  
            sra h  
            sra l  
            sra (hl)
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
        0xCB, 0x2F,
        0xCB, 0x28,
        0xCB, 0x29,
        0xCB, 0x2A,
        0xCB, 0x2B,
        0xCB, 0x2C,
        0xCB, 0x2D,
        0xCB, 0x2E,
        16, 0x00  
    ], data);
}

#[test]
fn swap() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            swap a
            swap b
            swap c
            swap d
            swap e
            swap h
            swap l
            swap (hl)
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
        0xCB, 0x37,
        0xCB, 0x30,
        0xCB, 0x31,
        0xCB, 0x32,
        0xCB, 0x33,
        0xCB, 0x34,
        0xCB, 0x35,
        0xCB, 0x36,
        16, 0x00   
    ], data);
}

#[test]
fn srl() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"        
            srl a
            srl b
            srl c
            srl d
            srl e
            srl h
            srl l
            srl (hl)
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
        0xCB, 0x3F,
        0xCB, 0x38,
        0xCB, 0x39,
        0xCB, 0x3A,
        0xCB, 0x3B,
        0xCB, 0x3C,
        0xCB, 0x3D,
        0xCB, 0x3E,
        16, 0x00  
    ], data);
}

#[test]
fn bit() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            bit 0, a
            bit 1, b
            bit 2, c
            bit 3, d
            bit 4, e
            bit 5, h
            bit 6, l
            bit 7, (hl)
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
        0xCB, 0x47,
        0xCB, 0x48,
        0xCB, 0x51,
        0xCB, 0x5A,
        0xCB, 0x63,
        0xCB, 0x6C,
        0xCB, 0x75,
        0xCB, 0x7E,
        16, 0x00
    ], data);
}

#[test]
fn res() {
    let assembler = assembler(&[(
        "/test.asm",
        r#" 
            res 0, a
            res 1, b
            res 2, c
            res 3, d
            res 4, e
            res 5, h
            res 6, l
            res 7, (hl)
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
        0xCB, 0x87,
        0xCB, 0x88,
        0xCB, 0x91,
        0xCB, 0x9A,
        0xCB, 0xA3,
        0xCB, 0xAC,
        0xCB, 0xB5,
        0xCB, 0xBE,
        16, 0x00
    ], data);
}

#[test]
fn set() {
    let assembler = assembler(&[(
        "/test.asm",
        r#" 
            set 0, a
            set 1, b
            set 2, c
            set 3, d
            set 4, e
            set 5, h
            set 6, l
            set 7, (hl)
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
        0xCB, 0xC7,
        0xCB, 0xC8,
        0xCB, 0xD1,
        0xCB, 0xDA,
        0xCB, 0xE3,
        0xCB, 0xEC,
        0xCB, 0xF5,
        0xCB, 0xFE,
        16, 0x00
    ], data);
}

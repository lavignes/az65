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
) -> Assembler<StringFileSystem, Cursor<String>, Z80Tokens, Z80> {
    Assembler::new(StringFileSystem::new(files), Z80)
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
            adc a, ixh
            adc a, ixl
            adc a, iyh
            adc a, iyl
            adc a, (hl)
            adc a, $42
            adc a, (ix+1)
            adc a, (iy+1)
            adc hl, bc
            adc hl, de
            adc hl, hl
            adc hl, sp
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
        0xDD, 0x8C,
        0xDD, 0x8D,
        0xFD, 0x8C,
        0xFD, 0x8D,
        0x8E,
        0xCE, 0x42,
        0xDD, 0x8E, 0x01,
        0xFD, 0x8E, 0x01,
        0xED, 0x4A,
        0xED, 0x5A,
        0xED, 0x6A,
        0xED, 0x7A,
        32, 0x00
    ], data);
}

#[test]
fn add() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            add a, a
            add a, b
            add a, c
            add a, d
            add a, e
            add a, h
            add a, l
            add a, ixh
            add a, ixl
            add a, iyh
            add a, iyl
            add a, (hl)
            add a, $42
            add a, (ix+1)
            add a, (iy+1)
            add hl, bc
            add hl, de
            add hl, hl
            add hl, sp
            add ix, bc
            add ix, de
            add ix, ix
            add ix, sp
            add iy, bc
            add iy, de
            add iy, iy
            add iy, sp
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
        0x87,
        0x80,
        0x81,
        0x82,
        0x83,
        0x84,
        0x85,
        0xDD, 0x84,
        0xDD, 0x85,
        0xFD, 0x84,
        0xFD, 0x85,
        0x86,
        0xC6, 0x42,
        0xDD, 0x86, 0x01,
        0xFD, 0x86, 0x01,
        0x09,
        0x19,
        0x29,
        0x39,
        0xDD, 0x09,
        0xDD, 0x19,
        0xDD, 0x29,
        0xDD, 0x39,
        0xFD, 0x09,
        0xFD, 0x19,
        0xFD, 0x29,
        0xFD, 0x39,
        44, 0x00
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
            and ixh
            and ixl
            and iyh
            and iyl
            and (hl)
            and $42
            and (ix+1)
            and (iy+1)
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
        0xDD, 0xA4,
        0xDD, 0xA5,
        0xFD, 0xA4,
        0xFD, 0xA5,
        0xA6,
        0xE6, 0x42,
        0xDD, 0xA6, 0x01,
        0xFD, 0xA6, 0x01,
        24, 0x00
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
            bit 0, (ix+1)
            bit 1, (iy+1)
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
        0xDD, 0xCB, 0x01, 0x46,
        0xFD, 0xCB, 0x01, 0x4E,
        24, 0x00
    ], data);
}

#[test]
fn call() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @org $0100
            test: nop
            call test
            call nz, test
            call z, test
            call nc, test
            call c, test
            call po, test
            call pe, test
            call p, test
            call m, test
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
        0xCD, 0x00, 0x01,
        0xC4, 0x00, 0x01,
        0xCC, 0x00, 0x01,
        0xD4, 0x00, 0x01,
        0xDC, 0x00, 0x01,
        0xE4, 0x00, 0x01,
        0xEC, 0x00, 0x01,
        0xF4, 0x00, 0x01,
        0xFC, 0x00, 0x01,
        28, 0x01
    ], data);
}

#[test]
fn ccf() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ccf
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
        0x3F,
        1, 0x00
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
            cp ixh
            cp ixl
            cp iyh
            cp iyl
            cp (hl)
            cp $42
            cp (ix+1)
            cp (iy+1)
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
        0xBF,
        0xB8,
        0xB9,
        0xBA,
        0xBB,
        0xBC,
        0xBD,
        0xDD, 0xBC,
        0xDD, 0xBD,
        0xFD, 0xBC,
        0xFD, 0xBD,
        0xBE,
        0xFE, 0x42,
        0xDD, 0xBE, 0x01,
        0xFD, 0xBE, 0x01,
        24, 0x00
    ], data);
}

#[test]
fn cpd() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            cpd
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
        0xED, 0xA9,
        2, 0x00
    ], data);
}

#[test]
fn cpdr() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            cpdr
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
        0xED, 0xB9,
        2, 0x00
    ], data);
}

#[test]
fn cpi() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            cpi
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
        0xED, 0xA1,
        2, 0x00
    ], data);
}

#[test]
fn cpir() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            cpir
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
        0xED, 0xB1,
        2, 0x00
    ], data);
}

#[test]
fn cpl() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            cpl
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
        0x2F,
        1, 0x00
    ], data);
}

#[test]
fn daa() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            daa
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
        1, 0x00
    ], data);
}

#[test]
fn dec() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            dec a
            dec b
            dec c
            dec d
            dec e
            dec h
            dec l
            dec ixh
            dec ixl
            dec iyh
            dec iyl
            dec bc
            dec de
            dec hl
            dec sp
            dec ix
            dec iy
            dec (hl)
            dec (ix+1)
            dec (iy+1)
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
        0x3D,
        0x05,
        0x0D,
        0x15,
        0x1D,
        0x25,
        0x2D,
        0xDD, 0x25,
        0xDD, 0x2D,
        0xFD, 0x25,
        0xFD, 0x2D,
        0x0B,
        0x1B,
        0x2B,
        0x3B,
        0xDD, 0x2B,
        0xFD, 0x2B,
        0x35,
        0xDD, 0x35, 0x01,
        0xFD, 0x35, 0x01,
        30, 0x00
    ], data);
}

#[test]
fn di() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            di
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
        0xF3,
        1, 0x00
    ], data);
}

#[test]
fn djnz() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @org 100
            bar: nop
            @org 0
            foo: djnz foo
            djnz bar
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
        0x10, -2i8 as u8,
        0x10, 96,
        4, 0x00
    ], data);
}

#[test]
fn ei() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ei
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
        0xFB,
        1, 0x00
    ], data);
}

#[test]
fn ex() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ex af, af'
            ex de, hl
            ex (sp), hl
            ex (sp), ix
            ex (sp), iy
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
        0xEB,
        0xE3,
        0xDD, 0xE3,
        0xFD, 0xE3,
        7, 0x00
    ], data);
}

#[test]
fn exx() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            exx
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
        0xD9,
        1, 0x00
    ], data);
}

#[test]
fn halt() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            halt
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
        0x76,
        1, 0x00
    ], data);
}

#[test]
fn im() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            im 0
            im 1
            im 2
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
        0xED, 0x46,
        0xED, 0x56,
        0xED, 0x5E,
        6, 0x00
    ], data);
}

#[test]
fn r#in() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @defn PORT, $42
            in a, (c)
            in b, (c)
            in c, (c)
            in d, (c)
            in e, (c)
            in h, (c)
            in l, (c)
            in a, (PORT)
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
        0xED, 0x78,
        0xED, 0x40,
        0xED, 0x48,
        0xED, 0x50,
        0xED, 0x58,
        0xED, 0x60,
        0xED, 0x68,
        0xDB, 0x42,
        16, 0x00
    ], data);
}

#[test]
fn inc() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            inc a
            inc b
            inc c
            inc d
            inc e
            inc h
            inc l
            inc ixh
            inc ixl
            inc iyh
            inc iyl
            inc bc
            inc de
            inc hl
            inc sp
            inc ix
            inc iy
            inc (hl)
            inc (ix+1)
            inc (iy+1)
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
        0x3C,
        0x04,
        0x0C,
        0x14,
        0x1C,
        0x24,
        0x2C,
        0xDD, 0x24,
        0xDD, 0x2C,
        0xFD, 0x24,
        0xFD, 0x2C,
        0x03,
        0x13,
        0x23,
        0x33,
        0xDD, 0x23,
        0xFD, 0x23,
        0x34,
        0xDD, 0x34, 0x01,
        0xFD, 0x34, 0x01,
        30, 0x00
    ], data);
}

#[test]
fn ind() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ind
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
        0xED, 0xAA,
        2, 0x00
    ], data);
}

#[test]
fn indr() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            indr
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
        0xED, 0xBA,
        2, 0x00
    ], data);
}

#[test]
fn ini() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ini
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
        0xED, 0xA2,
        2, 0x00
    ], data);
}

#[test]
fn inir() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            inir
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
        0xED, 0xB2,
        2, 0x00
    ], data);
}

#[test]
fn jp() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @org $0100
            test: nop
            jp test
            jp nz, test
            jp z, test
            jp nc, test
            jp c, test
            jp po, test
            jp pe, test
            jp p, test
            jp m, test
            jp (hl)
            jp (ix)
            jp (iy)
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
        0xC3, 0x00, 0x01,
        0xC2, 0x00, 0x01,
        0xCA, 0x00, 0x01,
        0xD2, 0x00, 0x01,
        0xDA, 0x00, 0x01,
        0xE2, 0x00, 0x01,
        0xEA, 0x00, 0x01,
        0xF2, 0x00, 0x01,
        0xFA, 0x00, 0x01,
        0xE9,
        0xDD, 0xE9,
        0xFD, 0xE9,
        33, 0x01
    ], data);
}

#[test]
fn jr() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @org 100
            bar: nop
            @org 0
            foo: jr foo
            jr bar
            foo1: jr nz, foo1
            jr nz, bar
            foo2: jr z, foo2
            jr z, bar
            foo3: jr nc, foo3
            jr nc, bar
            foo4: jr c, foo4
            jr c, bar
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
        0x18, -2i8 as u8,
        0x18, 96,
        0x20, -2i8 as u8,
        0x20, 92,
        0x28, -2i8 as u8,
        0x28, 88,
        0x30, -2i8 as u8,
        0x30, 84,
        0x38, -2i8 as u8,
        0x38, 80,
        20, 0x00
    ], data);
}

#[test]
fn ld() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            test:
            ld a, a
            ld a, b
            ld a, c
            ld a, d
            ld a, e
            ld a, h
            ld a, l
            ld a, ixh
            ld a, ixl
            ld a, iyh
            ld a, iyl
            ld a, i
            ld a, r
            ld a, (bc)
            ld a, (de)
            ld a, (hl)
            ld a, (ix+1)
            ld a, (iy+1)
            ld a, (test)
            ld a, $42
            ld b, a
            ld b, b
            ld b, c
            ld b, d
            ld b, e
            ld b, h
            ld b, l
            ld b, ixh
            ld b, ixl
            ld b, iyh
            ld b, iyl
            ld b, (hl)
            ld b, (ix+1)
            ld b, (iy+1)
            ld b, $42
            ld c, a
            ld c, b
            ld c, c
            ld c, d
            ld c, e
            ld c, h
            ld c, l
            ld c, ixh
            ld c, ixl
            ld c, iyh
            ld c, iyl
            ld c, (hl)
            ld c, (ix+1)
            ld c, (iy+1)
            ld c, $42
            ld d, a
            ld d, b
            ld d, c
            ld d, d
            ld d, e
            ld d, h
            ld d, l
            ld d, ixh
            ld d, ixl
            ld d, iyh
            ld d, iyl
            ld d, (hl)
            ld d, (ix+1)
            ld d, (iy+1)
            ld d, $42
            ld e, a
            ld e, b
            ld e, c
            ld e, d
            ld e, e
            ld e, h
            ld e, l
            ld e, ixh
            ld e, ixl
            ld e, iyh
            ld e, iyl
            ld e, (hl)
            ld e, (ix+1)
            ld e, (iy+1)
            ld e, $42
            ld h, a
            ld h, b
            ld h, c
            ld h, d
            ld h, e
            ld h, h
            ld h, l
            ld h, (hl)
            ld h, (ix+1)
            ld h, (iy+1)
            ld h, $42
            ld l, a
            ld l, b
            ld l, c
            ld l, d
            ld l, e
            ld l, h
            ld l, l
            ld l, (hl)
            ld l, (ix+1)
            ld l, (iy+1)
            ld l, $42
            ld ixh, a
            ld ixh, b
            ld ixh, c
            ld ixh, d
            ld ixh, e
            ld ixh, ixh
            ld ixh, ixl
            ld ixh, $42
            ld ixl, a
            ld ixl, b
            ld ixl, c
            ld ixl, d
            ld ixl, e
            ld ixl, ixh
            ld ixl, ixl
            ld ixl, $42
            ld iyh, a
            ld iyh, b
            ld iyh, c
            ld iyh, d
            ld iyh, e
            ld iyh, iyh
            ld iyh, iyl
            ld iyh, $42
            ld iyl, a
            ld iyl, b
            ld iyl, c
            ld iyl, d
            ld iyl, e
            ld iyl, iyh
            ld iyl, iyl
            ld iyl, $42
            ld r, a
            ld i, a
            ld sp, hl
            ld sp, ix
            ld sp, iy
            ld sp, (test)
            ld sp, test
            ld bc, (test)
            ld bc, test
            ld de, (test)
            ld de, test
            ld hl, (test)
            ld hl, test
            ld ix, (test)
            ld ix, test
            ld iy, (test)
            ld iy, test
            ld (bc), a
            ld (de), a
            ld (hl), a
            ld (hl), b
            ld (hl), c
            ld (hl), d
            ld (hl), e
            ld (hl), h
            ld (hl), l
            ld (hl), $42
            ld (ix+1), a
            ld (ix+1), b
            ld (ix+1), c
            ld (ix+1), d
            ld (ix+1), e
            ld (ix+1), h
            ld (ix+1), l
            ld (ix+1), $42
            ld (iy+1), a
            ld (iy+1), b
            ld (iy+1), c
            ld (iy+1), d
            ld (iy+1), e
            ld (iy+1), h
            ld (iy+1), l
            ld (iy+1), $42
            ld (test), a
            ld (test), bc
            ld (test), de
            ld (test), hl
            ld (test), sp
            ld (test), ix
            ld (test), iy
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
        0xDD, 0x7C,
        0xDD, 0x7D,
        0xFD, 0x7C,
        0xFD, 0x7D,
        0xED, 0x57,
        0xED, 0x5F,
        0x0A,
        0x1A,
        0x7E,
        0xDD, 0x7E, 0x01,
        0xFD, 0x7E, 0x01,
        0x3A, 0x00, 0x00,
        0x3E, 0x42,
        0x47,
        0x40,
        0x41,
        0x42,
        0x43,
        0x44,
        0x45,
        0xDD, 0x44,
        0xDD, 0x45,
        0xFD, 0x44,
        0xFD, 0x45,
        0x46,
        0xDD, 0x46, 0x01,
        0xFD, 0x46, 0x01,
        0x06, 0x42,
        0x4F,
        0x48,
        0x49,
        0x4A,
        0x4B,
        0x4C,
        0x4D,
        0xDD, 0x4C,
        0xDD, 0x4D,
        0xFD, 0x4C,
        0xFD, 0x4D,
        0x4E,
        0xDD, 0x4E, 0x01,
        0xFD, 0x4E, 0x01,
        0x0E, 0x42,
        0x57,
        0x50,
        0x51,
        0x52,
        0x53,
        0x54,
        0x55,
        0xDD, 0x54,
        0xDD, 0x55,
        0xFD, 0x54,
        0xFD, 0x55,
        0x56,
        0xDD, 0x56, 0x01,
        0xFD, 0x56, 0x01,
        0x16, 0x42,
        0x5F,
        0x58,
        0x59,
        0x5A,
        0x5B,
        0x5C,
        0x5D,
        0xDD, 0x5C,
        0xDD, 0x5D,
        0xFD, 0x5C,
        0xFD, 0x5D,
        0x5E,
        0xDD, 0x5E, 0x01,
        0xFD, 0x5E, 0x01,
        0x1E, 0x42,
        0x67,
        0x60,
        0x61,
        0x62,
        0x63,
        0x64,
        0x65,
        0x66,
        0xDD, 0x66, 0x01,
        0xFD, 0x66, 0x01,
        0x26, 0x42,
        0x6F,
        0x68,
        0x69,
        0x6A,
        0x6B,
        0x6C,
        0x6D,
        0x6E,
        0xDD, 0x6E, 0x01,
        0xFD, 0x6E, 0x01,
        0x2E, 0x42,
        0xDD, 0x67,
        0xDD, 0x60,
        0xDD, 0x61,
        0xDD, 0x62,
        0xDD, 0x63,
        0xDD, 0x64,
        0xDD, 0x65,
        0xDD, 0x26, 0x42,
        0xDD, 0x6F,
        0xDD, 0x68,
        0xDD, 0x69,
        0xDD, 0x6A,
        0xDD, 0x6B,
        0xDD, 0x6C,
        0xDD, 0x6D,
        0xDD, 0x2E, 0x42,
        0xFD, 0x67,
        0xFD, 0x60,
        0xFD, 0x61,
        0xFD, 0x62,
        0xFD, 0x63,
        0xFD, 0x64,
        0xFD, 0x65,
        0xFD, 0x26, 0x42,
        0xFD, 0x6F,
        0xFD, 0x68,
        0xFD, 0x69,
        0xFD, 0x6A,
        0xFD, 0x6B,
        0xFD, 0x6C,
        0xFD, 0x6D,
        0xFD, 0x2E, 0x42,
        0xED, 0x4F,
        0xED, 0x47,
        0xF9,
        0xDD, 0xF9,
        0xFD, 0xF9,
        0xED, 0x7B, 0x00, 0x00,
        0x31, 0x00, 0x00,
        0xED, 0x4B, 0x00, 0x00,
        0x01, 0x00, 0x00,
        0xED, 0x5B, 0x00, 0x00,
        0x11, 0x00, 0x00,
        0x2A, 0x00, 0x00,
        0x21, 0x00, 0x00,
        0xDD, 0x2A, 0x00, 0x00,
        0xDD, 0x21, 0x00, 0x00,
        0xFD, 0x2A, 0x00, 0x00,
        0xFD, 0x21, 0x00, 0x00,
        0x02,
        0x12,
        0x77,
        0x70,
        0x71,
        0x72,
        0x73,
        0x74,
        0x75,
        0x36, 0x42,
        0xDD, 0x77, 0x01,
        0xDD, 0x70, 0x01,
        0xDD, 0x71, 0x01,
        0xDD, 0x72, 0x01,
        0xDD, 0x73, 0x01,
        0xDD, 0x74, 0x01,
        0xDD, 0x75, 0x01,
        0xDD, 0x36, 0x01, 0x42,
        0xFD, 0x77, 0x01,
        0xFD, 0x70, 0x01,
        0xFD, 0x71, 0x01,
        0xFD, 0x72, 0x01,
        0xFD, 0x73, 0x01,
        0xFD, 0x74, 0x01,
        0xFD, 0x75, 0x01,
        0xFD, 0x36, 0x01, 0x42,
        0x32, 0x00, 0x00,
        0xED, 0x43, 0x00, 0x00,
        0xED, 0x53, 0x00, 0x00,
        0x22, 0x00, 0x00,
        0xED, 0x73, 0x00, 0x00,
        0xDD, 0x22, 0x00, 0x00,
        0xFD, 0x22, 0x00, 0x00,
        112, 0x01
    ], data);
}

#[test]
fn ldd() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ldd
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
        0xED, 0xA8,
        2, 0x00
    ], data);
}

#[test]
fn lddr() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            lddr
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
        0xED, 0xB8,
        2, 0x00
    ], data);
}

#[test]
fn ldi() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ldi
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
        0xED, 0xA0,
        2, 0x00
    ], data);
}

#[test]
fn ldir() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ldir
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
        0xED, 0xB0,
        2, 0x00
    ], data);
}

#[test]
fn neg() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            neg
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
        0xED, 0x44,
        2, 0x00
    ], data);
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
            or ixh
            or ixl
            or iyh
            or iyl
            or (hl)
            or $42
            or (ix+1)
            or (iy+1)
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
        0xDD, 0xB4,
        0xDD, 0xB5,
        0xFD, 0xB4,
        0xFD, 0xB5,
        0xB6,
        0xF6, 0x42,
        0xDD, 0xB6, 0x01,
        0xFD, 0xB6, 0x01,
        24, 0x00
    ], data);
}

#[test]
fn otdr() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            otdr
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
        0xED, 0xBB,
        2, 0x00
    ], data);
}

#[test]
fn otir() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            otir
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
        0xED, 0xB3,
        2, 0x00
    ], data);
}

#[test]
fn out() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @defn PORT, $42
            out (c), a
            out (c), b
            out (c), c
            out (c), d
            out (c), e
            out (c), h
            out (c), l
            out (PORT), a
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
        0xED, 0x79,
        0xED, 0x41,
        0xED, 0x49,
        0xED, 0x51,
        0xED, 0x59,
        0xED, 0x61,
        0xED, 0x69,
        0xD3, 0x42,
        16, 0x00
    ], data);
}

#[test]
fn outd() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            outd
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
        0xED, 0xAB,
        2, 0x00
    ], data);
}

#[test]
fn outi() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            outi
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
        0xED, 0xA3,
        2, 0x00
    ], data);
}

#[test]
fn pop() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            pop bc
            pop de
            pop hl
            pop af
            pop ix
            pop iy
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
        0xDD, 0xE1,
        0xFD, 0xE1,
        8, 0x00
    ], data);
}

#[test]
fn push() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            push bc
            push de
            push hl
            push af
            push ix
            push iy
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
        0xC5,
        0xD5,
        0xE5,
        0xF5,
        0xDD, 0xE5,
        0xFD, 0xE5,
        8, 0x00
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
            res 0, (ix+1)
            res 1, (iy+1)
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
        0xDD, 0xCB, 0x01, 0x86,
        0xFD, 0xCB, 0x01, 0x8E,
        24, 0x00
    ], data);
}

#[test]
fn ret() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            ret nz
            ret z
            ret nc
            ret c
            ret po
            ret pe
            ret p
            ret m
            ret
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
        0xC8,
        0xD0,
        0xD8,
        0xE0,
        0xE8,
        0xF0,
        0xF8,
        0xC9,
        9, 0x00
    ], data);
}

#[test]
fn reti() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
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
        0xED, 0x4D,
        2, 0x00
    ], data);
}

#[test]
fn retn() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            retn
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
        0xED, 0x45,
        2, 0x00
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
            rl (ix+1)
            rl (iy+1)
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
        0xDD, 0xCB, 0x01, 0x16,
        0xFD, 0xCB, 0x01, 0x16,
        24, 0x00
    ], data);
}

#[test]
fn rla() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            rla
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
        0x17,
        1, 0x00
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
            rlc (ix+1)
            rlc (iy+1)
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
        0xDD, 0xCB, 0x01, 0x06,
        0xFD, 0xCB, 0x01, 0x06,
        24, 0x00
    ], data);
}

#[test]
fn rlca() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            rlca
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
        1, 0x00
    ], data);
}

#[test]
fn rld() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            rld
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
        0xED, 0x6F,
        2, 0x00
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
            rr (ix+1)
            rr (iy+1)
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
        0xDD, 0xCB, 0x01, 0x1E,
        0xFD, 0xCB, 0x01, 0x1E,
        24, 0x00
    ], data);
}

#[test]
fn rra() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
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
        0x1F,
        1, 0x00
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
            rrc (ix+1)
            rrc (iy+1)
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
        0xDD, 0xCB, 0x01, 0x0E,
        0xFD, 0xCB, 0x01, 0x0E,
        24, 0x00
    ], data);
}

#[test]
fn rrca() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            rrca
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
        0x0F,
        1, 0x00
    ], data);
}

#[test]
fn rrd() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            rrd
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
        0xED, 0x67,
        2, 0x00
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
            sbc a, ixh
            sbc a, ixl
            sbc a, iyh
            sbc a, iyl
            sbc a, (hl)
            sbc a, $42
            sbc a, (ix+1)
            sbc a, (iy+1)
            sbc hl, bc
            sbc hl, de
            sbc hl, hl
            sbc hl, sp
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
        0xDD, 0x9C,
        0xDD, 0x9D,
        0xFD, 0x9C,
        0xFD, 0x9D,
        0x9E,
        0xDE, 0x42,
        0xDD, 0x9E, 0x01,
        0xFD, 0x9E, 0x01,
        0xED, 0x42,
        0xED, 0x52,
        0xED, 0x62,
        0xED, 0x72,
        32, 0x00
    ], data);
}

#[test]
fn scf() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            scf
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
        0x37,
        1, 0x00
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
            set 0, (ix+1)
            set 1, (iy+1)
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
        0xDD, 0xCB, 0x01, 0xC6,
        0xFD, 0xCB, 0x01, 0xCE,
        24, 0x00
    ], data);
}

#[test]
fn sll() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            sll a
            sll b
            sll c
            sll d
            sll e
            sll h
            sll l
            sll (hl)
            sll (ix+1)
            sll (iy+1)
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
        0xDD, 0xCB, 0x01, 0x36,
        0xFD, 0xCB, 0x01, 0x36,
        24, 0x00
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
            sla (ix+1)
            sla (iy+1)
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
        0xDD, 0xCB, 0x01, 0x26,
        0xFD, 0xCB, 0x01, 0x26,
        24, 0x00
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
            sra (ix+1)
            sra (iy+1)
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
        0xDD, 0xCB, 0x01, 0x2E,
        0xFD, 0xCB, 0x01, 0x2E,
        24, 0x00
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
            srl (ix+1)
            srl (iy+1)
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
        0xDD, 0xCB, 0x01, 0x3E,
        0xFD, 0xCB, 0x01, 0x3E,
        24, 0x00
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
            sub ixh
            sub ixl
            sub iyh
            sub iyl
            sub (hl)
            sub $42
            sub (ix+1)
            sub (iy+1)
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
        0xDD, 0x94,
        0xDD, 0x95,
        0xFD, 0x94,
        0xFD, 0x95,
        0x96,
        0xD6, 0x42,
        0xDD, 0x96, 0x01,
        0xFD, 0x96, 0x01,
        24, 0x00
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
            xor ixh
            xor ixl
            xor iyh
            xor iyl
            xor (hl)
            xor $42
            xor (ix+1)
            xor (iy+1)
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
        0xDD, 0xAC,
        0xDD, 0xAD,
        0xFD, 0xAC,
        0xFD, 0xAD,
        0xAE,
        0xEE, 0x42,
        0xDD, 0xAE, 0x01,
        0xFD, 0xAE, 0x01,
        24, 0x00
    ], data);
}

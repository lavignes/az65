use std::{
    fmt,
    fmt::{Display, Formatter},
    io::{self, Cursor},
    path::{Path, PathBuf},
};

use fxhash::FxHashMap;

use super::*;
use crate::{
    fileman::FileSystem,
    lexer::{FlagName, OperationName, RegisterName},
};

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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct TestName;

impl Display for TestName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "nop")
    }
}

impl RegisterName for TestName {
    fn parse<S: AsRef<str>>(_: S) -> Option<Self> {
        None
    }
}

impl FlagName for TestName {
    fn parse<S: AsRef<str>>(_: S) -> Option<Self> {
        None
    }
}

impl OperationName for TestName {
    fn parse<S: AsRef<str>>(s: S) -> Option<Self> {
        match s.as_ref() {
            "nop" => Some(Self),
            _ => None,
        }
    }
}

#[derive(Copy, Clone)]
struct TestArchTokens;

impl ArchTokens for TestArchTokens {
    type RegisterName = TestName;
    type FlagName = TestName;
    type OperationName = TestName;
}

struct TestArchAssembler;

impl<S, R> ArchAssembler<S, R, TestArchTokens> for TestArchAssembler
where
    S: FileSystem<Reader = R>,
    R: Read,
{
    fn parse(
        asm: &mut Assembler<S, R, TestArchTokens, Self>,
        _: TestName,
    ) -> Result<(), (SourceLoc, AssemblerError)> {
        asm.next()?;
        asm.data.push(0);
        Ok(())
    }
}

fn assembler<P: AsRef<Path>>(
    files: &[(P, &str)],
) -> Assembler<StringFileSystem, Cursor<String>, TestArchTokens, TestArchAssembler> {
    Assembler::new(StringFileSystem::new(files), TestArchAssembler)
}

#[test]
fn macros1() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @macro test, 0
                nop
                nop
                nop
                @dw @here
            @endmacro

            test
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
        0x00,
        0x00,
        3, 0x00
    ], data);
}

#[test]
fn macros2() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @macro test, 1
                nop
                @1
                nop
                @dw @here
            @endmacro

            test flop
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
        0x00,
        2, 0x00
    ], data);
}

#[test]
fn macros3() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @macro test, 2
                nop
                @1
                @1
                @2
                nop
                @dw @here
            @endmacro

            test nop, nop
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
        0x00,
        0x00,
        0x00,
        0x00,
        5, 0x00
    ], data);
}

#[test]
fn macros4() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @macro test, 1
                nop
                @1
                nop
                @dw @here
            @endmacro

            test {nop nop nop}
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
        0x00,
        0x00,
        0x00,
        0x00,
        5, 0x00
    ], data);
}

#[test]
fn macros5() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @macro test1, 1
                @1
            @endmacro

            @macro test2, 0
                nop
                test1 nop
                nop
                @dw @here
            @endmacro

            test2
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
        0x00,
        0x00,
        3, 0x00
    ], data);
}

#[test]
fn macros6() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @macro test2, 0
                @@macro test1, 1
                    @@1
                @@endmacro

                nop
                test1 nop
                nop
            @endmacro

            test2
            test1 nop
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
        0x00,
        0x00,
        0x00,
        4, 0x00
    ], data);
}

#[test]
fn macros7() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @macro ASSERT_EQ, 2
                @assert @1 == @2
            @endmacro

            ASSERT_EQ 4, { 2 + 2 }
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();
}

#[test]
fn structs1() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @assert MyStruct.field3 == 6

            @struct MyStruct
                field1 2
                field2 1
                @ds 3
                field3 1
            @endstruct
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();
}

#[test]
fn structs2() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @assert MyStruct.field3 == 8 && MyStruct.field4 == 16

            @assert @sizeof MyStruct.field1 == 2
            @assert @sizeof MyStruct.field2 == 1
            @assert @sizeof MyStruct.field3 == 1
            @assert @sizeof MyStruct.field4 == 1

            @struct MyStruct
                field1 2
                field2 1
                @align 8
                field3 1
                @align 8
                field4 1
            @endstruct

            @macro SIZEOF, 1
                @@parse @@getmeta @1, "@SIZEOF"
            @endmacro
            
            @assert 2 == SIZEOF MyStruct.field1
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();
}

#[test]
fn align() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @org $0000
            nop

            @align $1000
            @assert @here == $1000
            nop

            @align $1000
            @assert @here == $2000
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();
}

#[test]
fn stringify_and_labelify() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @echo @string test

            @echo @string { 1 " " 2 "hello" 2 + 2 }

            @label "test"

            @defl @label { global "." @string { the "_" local } }, 42
            @assert global.the_local == 42
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();
}

#[test]
fn stringify2() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @macro escaped_string, 1
                @echo @@string { @1 }
            @endmacro

            escaped_string { 1 + 2 + 3 }
        "#,
    )]);

    let mut data = Vec::new();
    assembler
        .assemble("/", "test.asm")
        .unwrap()
        .link(&mut data)
        .unwrap();
}

#[test]
fn each() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @macro MAKE_TABLE, 2
                @@label { @1 Lo }
                @@each { @2 }
                    @db <(@@1)
                @@endeach

                @@label { @1 Hi }
                @@each { @2 }
                    @db >(@@1)
                @@endeach
            @endmacro

            @defl First, $1234
            @defl Second, $5678
            @defl Third, $9abc

            MAKE_TABLE MyTable, {
                First
                Second
                Third
            }
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
        0x34,
        0x78,
        0xBC,
        0x12,
        0x56,
        0x9A,
    ], data);
}

#[test]
fn control_flow() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
           @each { @count !@isdef STDMACROS_INC }
               @@defn STDMACROS_INC, 0

               @@macro REPEAT, 2
                   @@@each { @@@count (@@1) }
                       @@2
                   @@@endeach
               @@endmacro

               @@macro IF, 2
                   @@@each { @@@count (@@1) != 0 }
                       @@2
                   @@@endeach
               @@endmacro

               @@macro IF_ELSE, 3
                   @@@each { @@@count (@@1) != 0 }
                       @@2
                   @@@endeach
                   @@@each { @@@count (@@1) == 0 }
                       @@3
                   @@@endeach
               @@endmacro

               @@macro ONCE, 2
                   @@@each { @@@count !@@@isdef @@1 }
                       @@@defn @@1, 0
                       @@2
                   @@@endeach
               @@endmacro
           @endeach

           REPEAT 3, {
               nop
           }

           IF { !@isdef Test }, {
               @echo "not defined"
           }

           @assert !@isdef Test
           @defn Test, 2
           @assert @isdef Test

           IF_ELSE { !@isdef Test }, {
               @echo "not defined"
           }, {
               @echo "defined"
           }

           ONCE _tok, { }
           ONCE _tok, { @die "impossible" }
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
        0x00,
        0x00,
    ], data);
}

#[test]
fn if_directive() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @if ! @isdef TEST
                nop
            @endif 
            
            @defn TEST, 1

            @if ! @isdef TEST
                @die "impossible!"
            @endif 

            @if @isdef TEST
                @if ! @isdef FOO
                    nop
                @endif
            @endif
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
        0x00,
    ], data);
}

#[test]
fn parse() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
           @parse "nop"
           nop
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
        0x00,
    ], data);
}

#[test]
fn getmeta() {
    let assembler = assembler(&[(
        "/test.asm",
        r##"
           @meta "BANK" "33"
           mylabel:
               nop

           @macro BANK_OF, 1
               @@parse @@string { "$" @@getmeta @1, "BANK" }
           @endmacro

           @db BANK_OF mylabel
        "##,
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
        0x33
    ], data);
}

#[test]
fn link_byte() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @db test
            @defn test, $42
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
        0x42,
    ], data);
}

#[test]
fn link_word() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @dw test
            @defn test, $1234
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
        0x34, 0x12
    ], data);
}

#[test]
fn link_space() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @ds 3, test
            @defn test, $42
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
        0x42, 0x42, 0x42
    ], data);
}

#[test]
fn code_segment() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @segment "CODE"
            nop
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
        0x00,
        2, 0x00,
    ], data);
}

#[test]
fn addr_segment() {
    let assembler = assembler(&[(
        "/test.asm",
        r#"
            @segment "ADDR"
            @db
            @dw
            @ds 3
            
            @segment "CODE"
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
        6, 0x00,
    ], data);
}

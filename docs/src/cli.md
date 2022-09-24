# Command Line Interface

All the functionality of AZ65 is in the `az65` binary.

## Assembling Files

Pass the target CPU architecture and the name of an assembly file to assemble
it:

```sh
az65 6502 code.asm
```

## Architectures

* `6502` for MOS 6502
* `z80` for Zilog Z80
* `sm83` for the Sharp LR35902 (a.k.a gbz80)

By default, `az65` will write the assembled program to `stdout`. You can direct
this to a file using the `>` operator:

```sh
az65 6502 code.asm > code.bin
```

You can alternatively use the `-o` option to pick an output file:

```sh
az65 z80 code.asm -o code.bin
```

## Search Paths

AZ65 supports specifying search paths for locating files that are referenced
in your code. Pass search paths by repeatedly using the `-I` option.

```sh 
az65 sm83 code.asm -I macros -I data > code.bin
```

When a file is referenced by name it will be searched for in the same directory
as the currently assembled file and if not found each include path will be
checked in the order given.

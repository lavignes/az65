# `@segment`

When AZ65 is parsing your assembly it is running in one of two segment *modes*:

## `CODE`

By default, AZ65 is running in the `CODE` segment mode. This means that
AZ65 is generating normal code.

For example when assembling z80 code and the assembler encounters:

```
@org 0

LoadAWith42:
    ld a, $42
```

It will emit the bytes `3E 42`. And add an entry in the assembler's symbol
table for the value `LoadAWith42` set to the value `0` (the value of the program
counter).

## `ADDR`

However, if you switch to the `ADDR` segment mode the assembler is much
more restrictive in what it accepts.

In the following example we add the directive `@segment` with a value of
`"ADDR"` to the top of our code:

```
@segment "ADDR"

@org 0

LoadAWith42:
    ld a, $42
```

This will result in an _ERROR_ indicating that instructions are not
allowed in the `ADDR` segment.

In this mode, you are free to define labels and constants, but
cannot generate any code. All directives work as normal besides
the `@db`, `@dw`, and `@ds` directives. In `ADDR` mode they only
increment the program counter.

The `ADDR` segment is meant specifically for defining memory addresses
in RAM. This is useful in systems where your code is stored in ROM,
usually game consoles like the Nintendo Entertainment System or Game Boy.

For example, the Game Boy memory map defines RAM as starting at `$C000`.
Rather than manually using `@defl` to manually define memory locations
for variables, you can let the assembler assign addresses:

```
@segment "ADDR"

org $C000

WRAM0:
    .PlayerX: @db
    .PlayerY: @db
    .PlayerHealth: @db
```

With this, you can easily define, move, and reference variables in RAM bank 0.

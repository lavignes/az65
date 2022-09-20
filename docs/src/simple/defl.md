# `@defl`

Use `@defl` to define labels that reference addresses.

Prefer using `@defl` instead of [`@defn`](./defn.md) for defining things such
as addresses in RAM. The assembler will treat `@defl` values as addresses for
example when generating debugging information.

`@defl` takes two arguments: a label, followed by an expression.

## Examples

```
@defl VRAM, $2000
@defl VRAM.palette, VRAM + $0100

@org $8000

ldx #$10
lda #$ff
sta VRAM.palette, x
```

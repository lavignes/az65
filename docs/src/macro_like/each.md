# `@each`

The `@each` directive will repeat a block of tokens
for each input token.

## Examples

```
; Will print the numbers on separate lines
@each Token, { 1 2 3 4 }
    @echo Token
@endeach
```

Each can be combined with [`@count`](./count.md) to create
a `REPEAT` macro:

```
@macro REPEAT, 2, N, Body
    @each _, { @count N }
        Body
    @endeach
@endmacro

; Prints "Hello" 3 times
REPEAT 3, {
    @echo "Hello"
}
```

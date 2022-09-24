# `@each`

The `@each` directive will repeat a block of tokens
for each input token.

## Examples

```
; Will print the numbers on separate lines
@each { 1 2 3 4 }
    @echo @1
@endeach
```

Each can be combined with [`@count`](./count.md) to create
a `REPEAT` macro:

```
@macro REPEAT, 2
    @@each { @@count @1 }
        @2
    @@endeach
@endmacro

; Prints "Hello" 3 times
REPEAT 3, {
    @echo "Hello"
}
```

You can also think of a `REPEAT` with a conditional
expression as an `IF` block for conditional compilation:

```
@macro IF, 2
    @@each { @@count (@1) == 1 }
        @2
    @@endeach
@endmacro

; Block of code will not be parsed
IF { 2 + 2 == 5 }, {
    @echo "2 + 2 == 5 !!"
}

; But this block will
IF { 2 + 2 == 4 }, {
    @echo "2 + 2 == 4 !!"
}
```

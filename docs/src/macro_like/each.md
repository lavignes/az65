# `@each`

The `@each` directive will repeat a block of tokens
for each input token.

## Examples

```
@each { 1 2 3 4 } ; Will print the numbers on separate lines
    @echo @1
@endeach
```

Each can be combined with count to create a `REPEAT` macro:

```
@macro REPEAT, 2
    @@each { @@count @1 }
        @2
    @@endeach
@endmacro

REPEAT 3, {
    @echo "Hello" ; Prints "Hello" 3 times
}
```

# `@ds`

`@ds` is used to define *space*, or really a contiguous sequence of bytes.
It along with its siblings: [`@db`](./db.md) and [`@dw`](./dw.md)
can have special meaning depending on where and when they are used.

## Examples

Most commonly, you'll use `@ds` to define strings in the `CODE`
[segment](./segment.md) of your binary:

```
@ds 8 ; Will add 8 bytes of $00 to the output.
```

However you can also define your space to have an initial value:

```
@db 16, $ff ; Will add 16 bytes of $ff to the output.
```

## Examples

Within the `ADDR` [segment](./segment.md), `@ds` simply increments the program
counter by a fixed amount.

```
    @segment "ADDR"
    @org 0

location:
    @ds 7
location2:
    @ds 1

    @assert location2 == 7
```


This works similarly in [structs](./struct.md) where it can be
used to insert padding:

```
@struct MyStruct
    @ds 2
    field 1
@endstruct

@assert MyStruct.field == 2
```

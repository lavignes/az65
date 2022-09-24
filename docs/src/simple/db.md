# `@db`

`@db` is used to define bytes. It along with its siblings:
[`@dw`](./dw.md) and [`@ds`](./ds.md) can have special meaning
depending on where and when they are used.

## Examples

Most commonly, you'll use `@db` to define strings in the `CODE`
[segment](./segment.md) of your binary:

```
@db "hello"
```

However you can also define sequences of bytes:

```
@db $42, $43, $44, $45
```

Or mix and match:

```
@db "Hello World", $a, "This is a test", $42
```

Within the `ADDR` [segment](./segment.md), `@db` simply increments the program
counter by 1.

```
    @segment "ADDR"
    @org 0

location:
    @db
location2:
    @db

    @assert location2 == 1
```


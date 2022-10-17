# `@dw`

`@dw` is used to define 16-bit "words". It along with its siblings:
[`@db`](./dw.md) and [`@ds`](./ds.md) can have special meaning
depending on where and when they are used.

## Examples

Most commonly, you'll use `@dw` to define words in the `CODE`
[segment](./segment.md) of your binary:

```
@dw $1234
```

Like with `@db`, you can define a sequence of words:

```
@dw $1234, $5678
```

Within the `ADDR` [segment](./segment.md), `@dw` simply increments the program
counter by 2.

```
    @segment "ADDR"
    @org 0

location:
    @dw
location2:
    @dw

    @assert location2 == 2
```


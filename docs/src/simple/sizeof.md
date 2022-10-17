# `@sizeof`

`@sizeof` is helper for accessing the expression provided
for labels defined in a [`@struct`](./struct.md). Normally,
when you have a struct like this:

```
@struct MyStruct
    field: 2
@endstruct
```

You have no way to access the result of the expression after
`field`. This size [*"metadata"*](./meta.md) is stored in the
symbol table, but you cannot access it directly. The label
`MyStruct.field` afterall is set to `0`: the offset of the
label from the start of the struct.

To access it easily, use `@sizeof`:

```
@defn FIELD_SIZE, @sizeof MyStruct.field
```

`@sizeof MyStruct.field` in this example will return `2`.

This has nearly the same effect as the much more clunky:

```
@defn FIELD_SIZE, @parse @string { @getmeta MyStruct.field, "@SIZEOF" "\\" }
```

Though `@sizeof` **acts as a unary operator in expressions**. It will be lazily
evaluated if necessary.

# `@struct`

The `@struct` directive is used to simplify working with
complex data types. *"Structs"* ultimately allow you to
define offsets and lengths of data from a base address.

We'll start with a simple example:

```
@struct MyStruct
    field1 1
    field2 2
@endstruct
```

A `@struct` directive begins with a global label. In this example
we create a struct named `MyStruct`. Then we provided a list of
label-expression pairs ending with an `@endstruct` directive.

This has the same effect as writing:

```
@segment "ADDR"
@org 0

MyStruct:

@meta "@SIZEOF" "1"
.field1: @ds 0
@endmeta

@meta "@SIZEOF" "2"
.field2: @ds 1
@endmeta

@redefl MyStruct, 1 + 2
```

We can ignore the [`@meta`](./meta.md) and [`@endmeta`](./meta.md)
directives for now and focus on the symbols added to the symbol table.

* `MyStruct` holds the sum of all expressions for each label defined
  in the struct. This is the "size" (or length) of the struct in bytes.
* `MyStruct.field1` holds the **offset** of the label in bytes from
  the start of the struct. The offset of a struct is the sum of all
  expressions that came before it. Sice it is the first label in the
  struct, it has an offset of `1`.
* `MyStruct.field2` as with the above, this label holds the offset
  of the label from the start of the struct. Since `.field1` evaluates
  to `1`, then `.field2` is set to `1`.

Structs provide a few other bits of syntactic sugar. Colons (`:`) following
labels within the struct body are optional. Expressions can contain references
to previously-defined labels in the struct. And you can use `@db`
and `@dw` as synonyms for `1` and `2`, respectively:

```
@struct MyStruct
    field1: @dw
    field2: .field1 + 1
    field3: @db
@endstruct
``` 

Finally, [`@align`](./align.md) can be used in structs to add padding
between labels just like you would do in normal code:

```
@struct MyStruct
    field1: @db

    @align 8
    field2: @db 
@endstruct
```

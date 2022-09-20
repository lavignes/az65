# `@assert`

The `@assert` directive is also invaluable for debugging and validating
invariants in your code.

## Examples

It is invoked with an expression argument and optional string message:

```
@assert 2 + 2 == 4
```

```
@assert 1, "cannot fail!"
```

Assertions support lazy evaluation. That means you can write assertions that
reference labels that are defined later in your code:

```
@assert SubRoutine.length == 4

SubRoutine:
    nop
    nop
    nop
    rts
    @defn .length, @here - SubRoutine
```
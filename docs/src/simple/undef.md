# `@undef`

Use `@undef` to "undefine" a label or constant.

## Examples

```
@defn TEST, 42

@echo TEST ; Prints "42"

@undef TEST

@echo TEST ; Will error due to expression not being solvable
```

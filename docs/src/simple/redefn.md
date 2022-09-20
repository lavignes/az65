# `@redefn`

[`@defn`](./defn.md) will not let you change the value of a constant once it is
defined. They are immutable.

Though there may be some circumstances you may want to do this. You can use
`@redefn` to achieve this.

## Examples

```
@defn CONST, 42
@defn CONST, 32 ; Will result in an ERROR!

@echo "Success!"
```

```
@defn CONST, 42
@redefn CONST, 32

@echo "Success!"
```
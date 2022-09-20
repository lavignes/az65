# `@org`

`@org` is the compliment to [`@here`](./here.md). It sets the virtual program
counter to a new 16-bit value.

## Examples

```
@org $8000
@echo @here ; Prints "32768"
```
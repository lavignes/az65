# `@here`

As AZ65 assembles your code, a virtual program counter keeps track of the
16-bit address of every instruction and label you write.

You access the current address in expressions using the `@here` directive.

## Examples

```
@echo @here ; Prints "0"
```

```
nop
nop
@echo @here ; Prints "2"
```

```
nop
nop
jmp @here - 2 ; Jumps 2 bytes back
```
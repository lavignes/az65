# `@defn`

Use `@defn` to define constants. This works similarly to `EQU` found in most
other assemblers.

`@defn` takes two arguments: a label, followed by an expression.

## Examples

```
@defn SCALE_FACTOR, 2

@echo 11 * SCALE_FACTOR ; Prints "22"
```

```
Global1: 
    @defn .LOCAL_CONSTANT, 2
    
Global2: 
    @defn .LOCAL_CONSTANT, 4

@echo Global1.LOCAL_CONSTANT ; Prints "2"

@echo Global2.LOCAL_CONSTANT ; Prints "4"
```

```
@defn Global.LOCAL_CONSTANT, 9
    
Global: 
    @echo .LOCAL_CONSTANT ; Prints "9"
```

# `@if`

The `@if` directive is used to essentually turn blocks of code
*ON* or *OFF*.

```
@if 0
    @die "Ignored!"	
@endif
```

An `@if` directive takes a single expression argument. If that expression
evaluates to `0` then all tokens are ignored by the assembler until it finds
a matching `@endif` directive. Any value other than `0` is interpreted as
being `true` and will result in the tokens being assembled as normal.

## Include Guards

Since AZ65 assembles and links your code in a single pass, you usually
dont need what are referred to as *include guards*. But for the sake
of demonstration, these are `@if` directives used in assembly languages
to prevent labels from being defined multiple times.

An example of an *include-guarded* assembly file would look like this:

```
; filename: "foo.inc"

@if ! @isdef FOO_INC
@defn FOO_INC, 1

@defl MY_CONSTANT, $42

@endif ; FOO_INC
```

In this example a constant value `FOO_INC` is used as a flag indicating
if the block of code within the `@if` and `@endif` directives has been
read by the assembler already. The [`@isdef`](../macro_like/isdef.md)
directive is used to check whether the `FOO_INC` flag has been defined,
if not, then it will be set to `1` on the next line.

This allows you to run `@include "foo.inc"` multiple times in your
assembly with no worry about errors.

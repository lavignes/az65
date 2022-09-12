# Expressions

## Number Formats

### Binary

Binary numbers in AZ65 use standard 6502 binary number syntax. They are prefixed
with a modulus (`%`). For example:
* `%00001111`
* `%1010`
* `%0`

### Hexadecimal

Like binary numbers, standard syntax is used. They are prefixed with a
dollar-sign (`$`) and are **case-insensitive**. For example:
* `$1234`
* `$DADcafe`
* `$0`

### Decimal

Numbers without a `%` or `$` prefix are assumed to be decimal (base 10) numbers.

## Operators

All expressions in AZ65 operate on 32-bit **signed** integers with wrapping
over/underflow semantics. All operators and their precedence match that of the C
language with a few notable modifications:

1. There is no C binary comma (`,`) operator. It is mostly an anachronism that
   many C programmers aren't even aware exists.
2. The unary `<` and `>` operators, common in 6502 assembly, are present. They
   are used to get the low and high byte of a 16-bit word. For example:
   * `< $1234` evaluates to `$34`.
   * `> $1234` evaluates to `$12`.
3. Unsigned (logical) shift operators are provided. Use the `<<<` and `>>>`
   symbols to shift left and right respectively:
   * `$ffffffff >>> 1` evaluates to `$7fffffff`
   * `$ffffffff <<< 1` evaluates to `$fffffffe`

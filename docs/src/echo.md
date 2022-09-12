# `@echo`

We'll start with the `@echo` directive since it is very useful for debugging
and demonstrating future directives.

The `@echo` directive takes a single string or expression argument and prints it
to `stderr`.

```
@echo "Hello World"
```

Note that expressions are always printed in base 10. 

```
@echo $1234 + $5678 ; Prints "26796" 
```

Methods of constructing strings out of expressions and printing in other bases
will be covered later.
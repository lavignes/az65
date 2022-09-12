# Strings

All strings in AZ64 are UTF-8 encoded. They are written enclosed in double
quotes (`"`):

* `"Hello World"`
* `"not a number: 1234"`
* `"Howdy, cowboy 🤠"`

Use C-style escape sequences to write special characters inside a string:

* `"line break: \n"`
* `"tab: \t"`
* `"double-quote: \""`

Multi-line strings can be written by placing a backslash immediately before the
line break:

```
"multi\
line\
string"
```

To encode a byte directly, place a backslash before a hexadecimal number:

```
"capital Q: \$51"
```
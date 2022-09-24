# Labels

There are 2 types of labels in AZ65:

## Global Labels

Global labels are labels as you'd normally expect them in an
assembler. They are alphanumeric tokens that are used to name
addresses and constants.

```
GlobalLabel:
    jr GlobalLabel
```

*Note that the use of colons (`:`) is optional*.

## Local Labels

Local labels are labels defined within the "scope" of
a global label-- that is labels that are defined after a
global label in your code. They look like global labels
but begin with a dot (`.`):

```
GlobalLabel:
    nop
.LocalLabel:
    jr .LocalLabel
```

Local labels are really just syntactic sugar for writing
longer *fully-qualified* labels. The example above is 
equivalent to this:

```
GlobalLabel:
    nop
GlobalLabel.LocalLabel:
    jp GlobalLabel.LocalLabel
```

This means that two global labels can have local labels
with the same name and they will not conflict with each other.

It also means you can always refer to a local label by its
full name. When written this way, they are referred to as
"direct" labels.

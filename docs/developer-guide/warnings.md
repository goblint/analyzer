# Warnings

## Types of warnings

Types of warnings form the following tree structure:

- Behavior (`behavior`)
  - Undefined (`undefined`)
    - ArrayOutOfBounds (`array_out_of_bounds`)
      - BeforeStart (`before_start`)
      - PastEnd (`past_end`)
      - Unknown (`unknown`)
    - NullPointerDereference (`nullpointer_dereference`)
    - UseAfterFree (`use_after_free`)
  - Machine (`machine`)
  - Implementation (`implementation`)
- Integer (`integer`)
  - Overflow (`overflow`)
  - DivByZero (`div_by_zero`)
- Cast (`cast`)
  - TypeMismatch (`type_mismatch`)
- Race (`race`)
- Analyzer (`analyzer`)
- Unknown (`unknown`)
- Debug (`debug`)

## Output

All warnings (except for `Debug`) are prepended with a tag `[Warning]`. This
enables easier detection in the test suite.

Next follows the tag determining certainty of the warning - either `[May]` or
`[Must]`.  After that, the category is printed - for example `[Behavior >
Undefined > NullPointerDereference]`. For some types there can be a custom
message after the category too (for example formatting some values passed to the
leaf variant or just printing out the warning more verbosely - see
ArrayOutOfBounds).

The optional message parameter of the functions is printed at the very end.

## OCaml

`Messages.warn` and `Messages.warn_each` can be used to print warnings from goblint.

To construct a nested variant, use one of the provided functions. Example for
`NullPointerDereference`:
`Messages.Warning.Behavior.Undefined.nullpointer_dereference ()`. The paths and
names correspond to the tree at the top of the page.

Other examples:

```
Messages.Unknown
Messages.Cast.type_mismatch ()
Messages.Integer.overlow ()
Messages.Behavior.Undefined.ArrayOutOfBounds.past_end ()
```

The warning type is given by an optional parameter `warning` (By default
`Unknown`). Both functions also have an optional parameter `must` (by default
`false` -> `May`) to determine certainty, a parameter `ctx` to gve context and
`msg` to pass an optional string message to print at the end of the warning.
Moreover, the `warn_each` function also has a parameter `loc` to supply location
to retain compatibility with former `report` function.

There's now no `Messages.report` function. It was replaced by `Messages.warn_each`.

Examples of calls to `warn` and `warn_each`:

```
Messages.warn (Messages.Warning.Integer.overflow ())
Messages.warn_each ~must:true (Messages.Warning.Behavior.Undefined.nullpointer_dereference ())
Messages.warn ~msg:"I don't know what type of warning this is" ()
Messages.warn ~must:true ~msg:"I don't know what type of warning this is" ()
Messages.warn ~msg:"my message" (Messages.Warning.Behavior.Undefined.nullpointer_dereference ())
Messages.warn_each ~loc:location ~msg:"my message" (Messages.Race)
```

## Spec files

Warnings inside spec files are also cocnverted to the warning types. Currently
the warnings are parsed from the string warnings. The first space delimited
group of characters is consumed and converted to a warning. The rest is passed
as an optional message.

Examples:

```
w1 "behavior.undefined.use_after_free"
w2 "integer.overflow"
w3 "unknown my message"
w4 "integer.overflow some text describing the warning"
```

The categories are given as dot delimited strings. For the possible values, see
the tree of categories at the top of the page - the strings in parentheses are
used in spec files.

Currently this spec file parsing doesn't support adding values of other types
the string message at the end. It probably doesn't even make sense in the spec
file as there's no way to pass runtime information into the warnings but a
possible extension would be to also parse parameters such as
`behavior.undefined.array_out_of_bounds(idx=3,len=2)`.

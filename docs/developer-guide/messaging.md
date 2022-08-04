# Messaging

The message system in `Messages` module should be used for outputting all (non-[tracing](./debugging.md#tracing)) information instead of printing them directly to `stdout`.
This allows for consistent pretty terminal output, as well as export to Goblint result viewers and IDEs.

## Message structure

A message consists of the following:

1. **Severity.** One of: error, warning, info, debug, success.
2. **Tags.** A list of tags (including multiple of the same kind):
    * **Category.** One of possibly-nested variants defined in `MessageCategory` module.
    * **CWE.** With a Common Weakness Enumeration number.
3. **Content.** One of the following:
    * **Single.** Contains the following:
        1. **Text.**
        2. **Location.** Optional.
        3. **Context.** Optional. Currently completely abstract, so not very useful.
    * **Group.** For messages related to numerous locations with different texts. Contains the following:
        1. **Group text.** An overall description of the group message.
        2. **Pieces.** A list of single messages as described above.

## Creating

### OCaml

In OCaml code, messages can be created using convenience functions in `Messages`.
For example:
```ocaml
Messages.warn "Text";
Messages.debug "Text"; (* severity functions *)
Messages.warn "Text %s %d %a" "foo" 42 Cil.d_exp exp; (* Pretty format *)
Messages.warn ~category:Analyzer "Text"; (* category *)
Messages.warn ~category:Messages.Category.Integer.overflow "Text"; (* category via helper *)
Messages.warn ~category:Messages.Category.Integer.overflow ~tags:[CWE 190] "Text"; (* extra tags *)
Messages.warn ~loc:otherloc "Text"; (* non-current location *)
Messages.warn_noloc "Text"; (* no location *)
```

The `~category` argument is optional and defaults to `Unknown`, but all newly added messages should have non-unknown category. New categories should be defined if necessary.
The `Messages.Category.` prefix is only necessary when using nested definitions from helper modules.

The `~tags` argument is optional and allows an arbitrary list of tags (including multiple different categories). The `~category` argument is simply for convenience to add one category tag.

The `~loc` argument is optional and defaults to the current location, but allows messages at a non-current location.

The `_noloc` suffixed functions allow general messages without any location (not even current).

By convention, may-warnings (the usual case) should use warning severity and  must-warnings should use error severity.

### Spec analysis

Warnings inside `.spec` files are converted to warnings.
They parsed from string warnings: the first space-delimited substring determines the category and the rest determines the text.

For example:
```
w1 "behavior.undefined.use_after_free"
w2 "integer.overflow"
w3 "unknown my message"
w4 "integer.overflow some text describing the warning"
```

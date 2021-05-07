# Debugging

## Printing
Goblint extensively uses [CIL's `Pretty`](https://people.eecs.berkeley.edu/~necula/cil/api/Pretty.html) module for printing due to many non-primitive values.

* Printing CIL values (e.g. an expression `exp`) using the corresponding pretty-printer `d_exp` from `Cil` module:

```ocaml
ignore (Pretty.printf "A CIL exp: %a\n" d_exp exp);
```

* Printing Goblint's `Printable` values (e.g. a domain `D` element `d`) using the corresponding pretty-printer `D.pretty`:

```ocaml
ignore (Pretty.printf "A domain element: %a\n" D.pretty d);
```

* Printing primitives (e.g. OCaml ints, strings, etc) using the standard [OCaml `Printf`](https://ocaml.org/api/Printf.html) specifiers:

```ocaml
ignore (Pretty.printf "An int and a string: %d %s\n" 42 "magic");
```

* Printing lists of pretty-printables (e.g. expressions list `exps`) using `d_list`:

```ocaml
ignore (Pretty.printf "Some expressions: %a\n" (d_list ", " d_exp) exps);
```


## Tracing
Tracing is a nicer alternative to debug printing, because it can be disabled for best performance and it can be used to only see relevant tracing output.

Recompile with tracing enabled: `./scripts/trace_on.sh`.

Instead of debug printing use a tracing function from the `Messages` module, which is often aliased to just `M` (and pick a relevant name instead of `mything`):
```ocaml
if M.tracing then M.trace "mything" "A domain element: %a\n" D.pretty d;
```

Then run Goblint with the additional argument `--trace mything`.
If the traced code runs often, it's best to pipe Goblint's output to a file.

Other tracing functions are available:

* `M.tracel` also includes the analysed program location.
* `M.tracei` and `M.traceu` can be used to indend and unindent tracing output.

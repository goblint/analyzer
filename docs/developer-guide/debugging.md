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

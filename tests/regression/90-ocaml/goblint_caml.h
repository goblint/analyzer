/* Redefinitions of caml macros for the C-stub analysis. This file replaces caml/memory.h in tests' imports. */

/* A small definition of the LXM state so sizeof works - from AI */
struct LXM_state { uint64_t a; uint64_t x[2]; uint64_t s; };

/* Minimal macros to mimic expected behaviour */
#define Wsizeof(ty) ((sizeof(ty) + sizeof(value) - 1) / sizeof(value))
#define LXM_val(v) ((struct LXM_state *) Data_abstract_val(v))

// Param and local macros redefined to register variables as GC roots, and CAMLreturn mocked to work with them.
#define CAMLparam0() __goblint_caml_param0()
#define CAMLparam1(x) __goblint_caml_param1(&x)
#define CAMLparam2(x, y) __goblint_caml_param2(&x, &y)
#define CAMLparam3(x, y, z) __goblint_caml_param3(&x, &y, &z)
#define CAMLparam4(x, y, z, w) __goblint_caml_param4(&x, &y, &z, &w)
#define CAMLparam5(x, y, z, w, v) __goblint_caml_param5(&x, &y, &z, &w, &v)

#define CAMLlocal1(x) value x = Val_unit; __goblint_caml_param1(&x) // The local and param functions behave the same for our purposes, registering variables.
#define CAMLlocal2(x, y) value x = Val_unit; value y = Val_unit; __goblint_caml_param2(&x, &y)
#define CAMLlocal3(x, y, z) value x = Val_unit; value y = Val_unit; value z = Val_unit; __goblint_caml_param3(&x, &y, &z)
#define CAMLlocal4(x, y, z, w) value x = Val_unit; value y = Val_unit; value z = Val_unit; value w = Val_unit; __goblint_caml_param4(&x, &y, &z, &w)
#define CAMLlocal5(x, y, z, w, v) value x = Val_unit; value y = Val_unit; value z = Val_unit; value w = Val_unit; value v = Val_unit; __goblint_caml_param5(&x, &y, &z, &w, &v)

#define CAMLreturn(x) return (x) // The real CAMLreturn needs some variable named caml__frame, which is not available in our redefinitions above.

// A reference to caml_gc_minor_words_unboxed can be found in _opam/lib/ocaml/ml/gc.ml.
#define caml_gc_minor_words_unboxed() (0.0)

// PARAM: --set "ana.activated[+]" ocaml --disable warn.imprecise --set "exp.extraspecials[+]" printInt

// Artificial test where the argument v is registered after GC could delete it.
// TODO: Add late local as well.

#include <stdint.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

CAMLprim value late_test(value v)
{
  value res = caml_alloc_small(1, Abstract_tag);
  CAMLparam1(v); // WARN
  memcpy((char *)&res, (char *)&v, sizeof(value)); // WARN
  CAMLreturn(res);
}
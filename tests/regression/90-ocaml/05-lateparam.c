// PARAM: --set "ana.activated[+]" ocaml --disable warn.imprecise --set "exp.extraspecials[+]" printInt

// Artificial test where the argument v is registered after GC could delete it.

#include <stdint.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include "goblint_caml.h"

CAMLprim value late_test(value v)
{
  value res = caml_alloc_small(Wsizeof(struct LXM_state), Abstract_tag);
  CAMLparam1(v); // WARN
  memcpy(LXM_val(res), LXM_val(v), sizeof(struct LXM_state)); // WARN
  CAMLreturn(res);
}
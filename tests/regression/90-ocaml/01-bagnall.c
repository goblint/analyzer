// PARAM: --set "ana.activated[+]" ocaml --set "mainfun[+]" "pringo_LXM_copy" --set "mainfun[+]" "pringo_LXM_copy_correct" --set "mainfun[+]" "pringo_LXM_init_unboxed" --disable warn.imprecise --set "exp.extraspecials[+]" printInt

// Buggy code from https://github.com/xavierleroy/pringo/issues/6 where value v is not registered.

#include <stdint.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include "goblint_caml.h"

CAMLprim value pringo_LXM_copy(value v)
{
  value res = caml_alloc_small(Wsizeof(struct LXM_state), Abstract_tag);
  memcpy(LXM_val(res), LXM_val(v), sizeof(struct LXM_state)); // WARN
  return res;
}

CAMLprim value pringo_LXM_copy_correct(value v)
{
  CAMLparam1(v);
  value res = caml_alloc_small(Wsizeof(struct LXM_state), Abstract_tag);
  memcpy(LXM_val(res), LXM_val(v), sizeof(struct LXM_state)); // NOWARN
  CAMLreturn(res);
}

CAMLprim value pringo_LXM_init_unboxed(uint64_t i1, uint64_t i2,
                                       uint64_t i3, uint64_t i4)
{
  value v = caml_alloc_small(Wsizeof(struct LXM_state), Abstract_tag); // NOWARN
  struct LXM_state * st = LXM_val(v);
  st->a = i1 | 1;    /* must be odd */
  st->x[0] = i2 != 0 ? i2 : 1; /* must be nonzero */
  st->x[1] = i3 != 0 ? i3 : 2; /* must be nonzero */
  st->s = i4;
  return v;
}
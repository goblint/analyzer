// PARAM: --set "ana.activated[+]" ocaml --disable warn.imprecise --set "exp.extraspecials[+]" printInt

// Artificial tests with one C-stub calling another inside it.

#include <stdint.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include "goblint_caml.h"

CAMLprim value pringo_LXM_copy(value v)
{ 
  value res1 = pringo_LXM_copy_correct(v); // NOWARN
  value res = caml_alloc_small(Wsizeof(struct LXM_state), Abstract_tag);
  memcpy(LXM_val(res), LXM_val(v), sizeof(struct LXM_state)); // WARN
  return res1; // WARN
}

CAMLprim value pringo_LXM_copy_correct(value v)
{
  CAMLparam1(v);
  value res = caml_alloc_small(Wsizeof(struct LXM_state), Abstract_tag);
  memcpy(LXM_val(res), LXM_val(v), sizeof(struct LXM_state)); // NOWARN
  CAMLreturn(res); // NOWARN
}

CAMLprim value pringo_LXM_copy_1(value v)
{ 
  value res = caml_alloc_small(Wsizeof(struct LXM_state), Abstract_tag);
  memcpy(LXM_val(res), LXM_val(v), sizeof(struct LXM_state)); // WARN
  return v; // WARN
}

CAMLprim value pringo_LXM_copy_2(value v)
{
  CAMLparam1(v);
  value res = caml_alloc_small(Wsizeof(struct LXM_state), Abstract_tag);
  v = pringo_LXM_copy_1(v); // WARN
  memcpy(LXM_val(res), LXM_val(v), sizeof(struct LXM_state)); // WARN
  CAMLreturn(res); // WARN
}

CAMLprim value pringo_LXM_copy_3(value v)
{
  value res = caml_alloc_small(Wsizeof(struct LXM_state), Abstract_tag);
  value r = pringo_LXM_copy_1(v); // WARN
  memcpy(LXM_val(res), LXM_val(r), sizeof(struct LXM_state)); // WARN
  CAMLreturn(res); // WARN
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

// If entering does not copy registration status, this function will give false positives.
CAMLprim value enter_test_1(value v)
{
  CAMLparam1(v);
  value res = enter_test_2(v); // NOWARN
  CAMLreturn(res); // NOWARN
}

CAMLprim value enter_test_2(value v)
{
  value res = caml_alloc_small(Wsizeof(struct LXM_state), Abstract_tag);
  // TODO: Inner function warns of memory leak. Are inner and outer registration the same or different? Investigate.
  return v; // WARN
}
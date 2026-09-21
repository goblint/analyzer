// PARAM: --set "ana.activated[+]" ocaml --disable warn.imprecise --set "exp.extraspecials[+]" printInt --set pre.cppflags[+] -D_FORTIFY_SOURCE=0 --set pre.cppflags[+] -O3

// Buggy code from https://github.com/xavierleroy/pringo/issues/6 where value v is not registered.

#include <stdint.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

/* A small definition of the LXM state so sizeof works - from AI */
struct LXM_state { uint64_t a; uint64_t x[2]; uint64_t s; };

/* Minimal macros to mimic expected behaviour */
#define Wsizeof(ty) ((sizeof(ty) + sizeof(value) - 1) / sizeof(value))
#define LXM_val(v) ((struct LXM_state *) Data_abstract_val(v))

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
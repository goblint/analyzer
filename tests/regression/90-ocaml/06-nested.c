// PARAM: --set "ana.activated[+]" ocaml --disable warn.imprecise --set "exp.extraspecials[+]" printInt

// Artificial tests with one C-stub calling another inside it.

#include <stdint.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

CAMLprim value pringo_LXM_copy(value v)
{
  value res1 = pringo_LXM_copy_correct(v); // NOWARN
  value res = caml_alloc_small(1, Abstract_tag);
  memcpy((char *)&res, (char *)&v, sizeof(value)); // WARN
  return res1; // WARN
}

CAMLprim value pringo_LXM_copy_correct(value v)
{
  CAMLparam1(v);
  value res = caml_alloc_small(1, Abstract_tag);
  memcpy((char *)&res, (char *)&v, sizeof(value)); // NOWARN
  CAMLreturn(res); // NOWARN
}

CAMLprim value pringo_LXM_copy_1(value v)
{
  value res = caml_alloc_small(1, Abstract_tag);
  memcpy((char *)&res, (char *)&v, sizeof(value)); // WARN
  return v; // WARN
}

CAMLprim value pringo_LXM_copy_2(value v)
{
  CAMLparam1(v);
  value res = caml_alloc_small(1, Abstract_tag);
  v = pringo_LXM_copy_1(v); // This warns only in the inner function.
  memcpy((char *)&res, (char *)&v, sizeof(value)); // WARN
  CAMLreturn(res); // WARN
}

CAMLprim value pringo_LXM_copy_3(value v)
{
  value res = caml_alloc_small(1, Abstract_tag);
  value r = pringo_LXM_copy_1(v); // WARN
  memcpy((char *)&res, (char *)&r, sizeof(value)); // WARN
  CAMLreturn(res); // WARN
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
  value res = caml_alloc_small(1, Abstract_tag);
  // TODO: Inner function warns of memory leak. Are inner and outer registration the same or different? Investigate.
  return v; // WARN
}
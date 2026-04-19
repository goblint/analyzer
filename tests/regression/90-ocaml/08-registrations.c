// PARAM: --set "ana.activated[+]" ocaml --disable warn.imprecise --set "exp.extraspecials[+]" printInt

// Artificial tests for assignment to values with different states of registration.

#include <stdint.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include "goblint_caml.h"

CAMLprim value registration_test_1(value v)
{
  CAMLparam1(v);
  CAMLlocal1(res);
  res = v;
  caml_alloc_small(Wsizeof(struct LXM_state), Abstract_tag);
  CAMLreturn(res); // NOWARN
}

// Though v is not directly registered, it is pointed to by a registered value, so it should not be garbage collected.
// If res is then deregistered, the status of v should follow. Changing the analysis to account for this case would be very hard.
CAMLprim value registration_test_2(value v)
{
  CAMLlocal1(res);
  res = v;
  caml_alloc_small(Wsizeof(struct LXM_state), Abstract_tag);
  CAMLreturn(res); // NOWARN
}

// While v will survive the garbage collection, res may no longer point to it.
CAMLprim value registration_test_3(value v)
{
  CAMLparam1(v);
  value res = v;
  caml_alloc_small(Wsizeof(struct LXM_state), Abstract_tag);
  CAMLreturn(res); // WARN
}

CAMLprim value registration_test_4(value v)
{
  value res = v;
  caml_alloc_small(Wsizeof(struct LXM_state), Abstract_tag);
  return res; // WARN
}

// Though v is indirectly registered via res, the analysis does not see this.
// This causes res2 to be in R, but not in A: a problem.
// TODO: Fix or count as limitation.
CAMLprim value registration_test_5(value v)
{
  CAMLlocal2(res, res2);
  res = v;
  caml_alloc_small(Wsizeof(struct LXM_state), Abstract_tag);
  res2 = v;
  CAMLreturn(res2); // NOWARN
}

// The memory leak test.
CAMLprim value registration_test_6(value v)
{
  CAMLparam1(v);
  CAMLlocal1(res);
  res = v;
  caml_alloc_small(Wsizeof(struct LXM_state), Abstract_tag);
  return res; // WARN
}
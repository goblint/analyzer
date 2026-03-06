// PARAM: --set "ana.activated[+]" ocaml --set "mainfun[+]" "branching_test" --disable warn.imprecise --set "exp.extraspecials[+]" printInt

// Artificial test where one branch registers the argument v and the other does not, but both branches use v.

#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include "goblint_caml.h"

CAMLprim value branching_test(value v, bool b)
{
    if (b)
    {
        CAMLparam1(v);
    }
    value res = caml_alloc_small(Wsizeof(struct LXM_state), Abstract_tag);
    memcpy(LXM_val(res), LXM_val(v), sizeof(struct LXM_state)); // WARN
    CAMLreturn(res);
}
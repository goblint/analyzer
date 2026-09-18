// PARAM: --set "ana.activated[+]" ocaml --disable warn.imprecise --set "exp.extraspecials[+]" printInt

// Artificial tests where one branch registers the argument v and the other does not, but both branches use v.

#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

CAMLprim value branching_test(value v, bool b)
{
    if (b)
    {
        CAMLparam1(v); // NOWARN
    }
    value res = caml_alloc_small(1, Abstract_tag);
    memcpy((char *)&res, (char *)&v, sizeof(value)); // WARN
    CAMLreturn(res);
}

CAMLprim value branching_test2(value v, bool b)
{
    if (b)
    {
        Begin_roots1(v); // NOWARN
    }
    value res = caml_alloc_small(1, Abstract_tag);
    memcpy((char *)&res, (char *)&v, sizeof(value)); // WARN
    if (b) // NOWARN
    {
        End_roots();
    }
    return res;
}
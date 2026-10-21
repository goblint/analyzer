// PARAM: --set "ana.activated[+]" ocaml --disable warn.imprecise --set "exp.extraspecials[+]" printInt

// Artificial tests dealing with arrays and pointers and dereferencing.

#include <stdint.h>
#include <string.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

static uintnat norm_window (intnat w)
{
  if (w < 1) w = 1;
  if (w > Max_major_window) w = Max_major_window;
  return w;
}

CAMLprim value look_fields(value v) // It cannot be known whether a value like this has fields.
{
    value res = Long_val (Field (v, 3));
    norm_window(res);
    return res;
}
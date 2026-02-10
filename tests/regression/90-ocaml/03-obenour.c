// PARAM: --set "ana.activated[+]" ocaml --set "mainfun[+]" "caml_gc_counters" --set "mainfun[+]" "caml_gc_counters_correct" --disable warn.imprecise --set "exp.extraspecials[+]" printInt

// Buggy code from https://github.com/ocaml/ocaml/pull/13370 where unregistered temporary variables may be garbage-collected.

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include "goblint_caml.h"

CAMLprim value caml_gc_counters(value v)
{
  CAMLparam0();   /* v is ignored */
  CAMLlocal1(res);

  /* get a copy of these before allocating anything... */
  double minwords = caml_gc_minor_words_unboxed();
  /*double prowords = (double)Caml_state->stat_promoted_words;
  double majwords = Caml_state->stat_major_words +
                    (double) Caml_state->allocated_words;*/
  double prowords = 0; // It does not find the Caml_state so dummy values are used for the test.
  double majwords = 0;

  res = caml_alloc_3(0,
    caml_copy_double(minwords),
    caml_copy_double(prowords),
    caml_copy_double(majwords)); // WARN
  CAMLreturn(res);
}

CAMLprim value caml_gc_counters_correct(value v)
{
  CAMLparam0(); /* v is ignored */
  CAMLlocal3(minwords_, prowords_, majwords_);

  /* get a copy of these before allocating anything... */
  double minwords = caml_gc_minor_words_unboxed();
  /*double prowords = (double)Caml_state->stat_promoted_words;
  double majwords = Caml_state->stat_major_words +
                    (double) Caml_state->allocated_words;*/
  double prowords = 0;
  double majwords = 0;

  minwords_ = caml_copy_double(minwords);
  prowords_ = caml_copy_double(prowords);
  majwords_ = caml_copy_double(majwords);
  v = caml_alloc_small(3, 0);
  Field(v, 0) = minwords_;
  Field(v, 1) = prowords_;
  Field(v, 2) = majwords_;
  CAMLreturn(v);
}
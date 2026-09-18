// PARAM: --set "ana.activated[+]" ocaml --disable warn.imprecise --set "exp.extraspecials[+]" printInt

// Buggy code from https://github.com/ocaml/ocaml/pull/13370 where unregistered temporary variables may be garbage-collected.

#define CAML_NAME_SPACE 1 // TODO: How to get Caml_state without this?
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

/* Caml_state is declared in lib/ocaml/caml/domain_state.h and exposed through
   caml/mlvalues.h; the OCaml headers name the fields without a leading underscore
   when CAML_NAME_SPACE is enabled, matching the raw field accesses in this test. */

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

CAMLprim value caml_gc_counters_correct_1(value v)
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

  minwords_ = caml_copy_double(minwords); // NOWARN
  prowords_ = caml_copy_double(prowords); // NOWARN
  majwords_ = caml_copy_double(majwords); // NOWARN
  v = caml_alloc_small(3, 0);
  Field(v, 0) = minwords_;
  Field(v, 1) = prowords_;
  Field(v, 2) = majwords_;
  CAMLreturn(v);
}

CAMLprim value caml_gc_counters_correct_2(value v)
{
  CAMLparam0(); /* v is ignored */
  CAMLlocal4(minwords_, prowords_, majwords_, res);

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
  res = caml_alloc_3(0, minwords_, prowords_, majwords_); // NOWARN
  CAMLreturn(res);
}

// TODO: Ensure Caml_state or whatever is never thought to be a null pointer.
double caml_gc_minor_words_unboxed (void)
{
  return (Caml_state->stat_minor_words
          + ((double) Wsize_bsize((uintnat)Caml_state->young_end -
                                  (uintnat)Caml_state->young_ptr)));
}
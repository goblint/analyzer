// PARAM: --set "ana.activated[+]" ocaml --set "mainfun[+]" "caml_gc_counters" --set "mainfun[+]" "caml_gc_counters_correct" --disable warn.imprecise --set "exp.extraspecials[+]" printInt

// OCaml code fixed by Demi Marie Obenour in https://github.com/ocaml/ocaml/pull/13370, before and after the fix.

/* Minimal macros to mimic expected behaviour */
#define Wsizeof(ty) ((sizeof(ty) + sizeof(value) - 1) / sizeof(value))
#define LXM_val(v) ((struct LXM_state *) Data_abstract_val(v))

#define CAMLparam0() __goblint_caml_param0()
#define CAMLparam1(x) __goblint_caml_param1(&x)
#define CAMLreturn(x) return (x) // From AI - CAMLreturn needs some variable named caml__frame, which is not available in our mock CAMLparam1, so we mock the return as well.


CAMLprim value caml_gc_counters(value v)
{
  CAMLparam0 ();   /* v is ignored */
  CAMLlocal1 (res);

  /* get a copy of these before allocating anything... */
  double minwords = caml_gc_minor_words_unboxed();
  double prowords = (double)Caml_state->stat_promoted_words;
  double majwords = Caml_state->stat_major_words +
                    (double) Caml_state->allocated_words;

  res = caml_alloc_3(0,
    caml_copy_double (minwords),
    caml_copy_double (prowords),
    caml_copy_double (majwords)); // WARN
  CAMLreturn (res);
}

CAMLprim value caml_gc_counters_correct(value v)
{
  CAMLparam0 (); /* v is ignored */
  CAMLlocal3 (minwords_, prowords_, majwords_);

  /* get a copy of these before allocating anything... */
  double minwords = caml_gc_minor_words_unboxed();
  double prowords = (double)Caml_state->stat_promoted_words;
  double majwords = Caml_state->stat_major_words +
                    (double) Caml_state->allocated_words;

  minwords_ = caml_copy_double(minwords);
  prowords_ = caml_copy_double(prowords);
  majwords_ = caml_copy_double(majwords);
  v = caml_alloc_small(3, 0);
  Field(v, 0) = minwords_;
  Field(v, 1) = prowords_;
  Field(v, 2) = majwords_;
  CAMLreturn(v);
}
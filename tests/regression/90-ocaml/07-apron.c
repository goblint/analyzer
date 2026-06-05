// PARAM: --set "ana.activated[+]" ocaml --disable warn.imprecise --set "exp.extraspecials[+]" printInt

// Buggy code from https://github.com/antoinemine/apron/pull/112 where v and v2 are initialised as 0.
// TODO: It should warn when a value initialised as 0 is used without being registered after a garbage collection.

#include <stdint.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include "goblint_caml.h"

// In place of value, it was ap_policy_optr*.
CAMLprim value camlidl_apron_policy_optr_c2ml(value p)
{
  if (p==NULL){
    return Val_int(0);
  } else {
    value v,v2=0;
    Begin_roots1(v2); // WARN
    v2 = camlidl_apron_policy_ptr_c2ml(p);
    v = caml_alloc_small(1,0);
    Field(v,0) = v2;
    End_roots();
    return v;
  }
}

CAMLprim value camlidl_apron_policy_optr_c2ml_correct(value p)
{
  if (p==NULL){
    return Val_int(0);
  } else {
    value v,v2 = Val_unit;
    Begin_roots1(v2); // NOWARN
    v2 = camlidl_apron_policy_ptr_c2ml(p);
    v = caml_alloc_small(1,0);
    Field(v,0) = v2;
    End_roots();
    return v;
  }
}


// This function basically allocates memory with caml_alloc_custom and is simulated with OCamlParam in libraryfunctions.ml.
/*CAMLprim value camlidl_apron_policy_ptr_c2ml(ap_policy_ptr* p)
{
  value v;
  assert((*p)->pman!=NULL);
  v = caml_alloc_custom(&camlidl_apron_custom_policy_ptr, sizeof(ap_policy_ptr),
		   0,1);
  *((ap_policy_ptr *) Data_custom_val(v)) = *p;
  return v;
}*/
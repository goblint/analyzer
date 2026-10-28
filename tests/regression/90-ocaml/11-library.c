// PARAM: --set "ana.activated[+]" ocaml --disable warn.imprecise --set "exp.extraspecials[+]" printInt

// Artificial tests using libraryFunctions to gauge whether they're handled correctly.

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

// Includes from osdeps.h
extern char *caml_secure_getenv(char const *var);
extern char *secure_getenv(char const *name);

// The variable shm_id_str should be accounted for.
CAMLprim value get_an_env(value unit)
{
  char* shm_id_str;
  shm_id_str = caml_secure_getenv("__AFL_SHM_ID");
  if (shm_id_str == NULL) {
    return Val_unit;
  }
  return unit;
}

char *caml_secure_getenv(char const *var)
{
  return secure_getenv(var);
}

static unsigned char afl_area_initial[1 << 16];
unsigned char* caml_afl_area_ptr = afl_area_initial;

CAMLprim value caml_reset_afl_instrumentation(value full)
{
  if (full != Val_int(0)) {
    memset(caml_afl_area_ptr, 0, sizeof(afl_area_initial));
  }
  return Val_unit;
}
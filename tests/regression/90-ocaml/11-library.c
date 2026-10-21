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

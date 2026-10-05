// PARAM: --set "ana.activated[+]" ocaml --disable warn.imprecise --set "exp.extraspecials[+]" printInt

#include <stdint.h>
#include <string.h>
#include <caml/alloc.h>
#include <caml/config.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

CAMLprim value pringo_chacha_make_key(value vkey)
{
  int keylen;
  uint8_t keybytes[32];
  value res;

  keylen = caml_string_length(vkey);
  if (keylen > 32) keylen = 32;
  memcpy(keybytes, String_val(vkey), keylen); // NOWARN
  memset(keybytes + keylen, 0, 32 - keylen); // NOWARN
  res = caml_alloc_small(1, Abstract_tag);
  chacha20_init_key(Key_val(res), keybytes, keylen <= 16 ? 16 : 32); // TODO NOWARN
  memset(keybytes, 0, 32);      /* just in case key is sensitive */ // NOWARN
  return res;
}
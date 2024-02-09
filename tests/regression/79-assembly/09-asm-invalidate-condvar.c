// PARAM: --set ana.activated[+] condvars --set ana.activated[+] taintPartialContexts --disable asm_is_nop
#include <goblint.h>

int glob;

void f() {
}

int main() {
  int unk;
  int tv;
  if (unk)
    glob = 0;
  else
    glob = 10;

  tv = (glob == 0);
  f();
  
  __asm__("nop": "=x"(glob): "x="(glob));

  if (tv)
    __goblint_assert(glob == 0); //UNKNOWN
  else 
    __goblint_assert(glob != 0); //UNKNOWN

}


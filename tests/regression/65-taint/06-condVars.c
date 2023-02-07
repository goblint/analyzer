// PARAM: --set ana.activated[+] condvars --set ana.activated[+] taintPartialContexts
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

  if (tv)
    __goblint_assert(glob == 0);
  else 
    __goblint_assert(glob != 0);

}

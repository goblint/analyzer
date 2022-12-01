// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --enable ana.int.interval
#include <goblint.h>

int main(void) {
  unsigned int x = 10;

  while (x >= 10) {
    x += 2;
  }

  __goblint_check(1);
}

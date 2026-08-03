// PARAM: --set ana.activated[+] apron
#include <goblint.h>
static void f(int x, int *outer) {
  if (outer) {
    __goblint_check(*outer == 2); // UNKNOWN! (outer cell is still 2; must not disprove it)
  }
  else f(1, &x);
}
int main(void) { f(2, 0); return 0; }

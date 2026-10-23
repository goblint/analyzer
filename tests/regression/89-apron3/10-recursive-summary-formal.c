// PARAM: --set ana.activated[+] apron
#include <goblint.h>
static void f(int x, int *outer) {
  if (outer) {
    __goblint_check(*outer == 2); // outer activation still holds 2
  }
  else f(1, &x);
}
int main(void) { f(2, 0); return 0; }

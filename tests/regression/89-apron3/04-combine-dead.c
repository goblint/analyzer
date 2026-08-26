// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] base --set ana.ctx_insens[+] base --set ana.ctx_insens[+] apron --enable ana.int.interval
// NOCRASH
#include <goblint.h>

int f(int x) {
  return x + 1;
}

int main() {
  int a = f(1);
  __goblint_check(a == 2);
  int b = f(2);
  __goblint_check(b == 3);
  return 0;
}

// PARAM: --set ana.activated[+] threadAccess
#include <signal.h>
#include <goblint.h>

int g = 0;

void handler(int sig) {
}

int main() {
  __goblint_check(g == 0);
  g = 1;
  __goblint_check(g == 1);
  signal(SIGTERM, handler);
  __goblint_check(g == 1);
  g = 2;
  __goblint_check(g == 2);
  return 0;
}

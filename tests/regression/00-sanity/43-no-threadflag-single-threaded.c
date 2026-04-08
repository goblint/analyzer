// PARAM: --set ana.activated[-] threadflag --set ana.activated[-] threadid --disable exp.earlyglobs --enable exp.single-threaded
// Like 00-sanity/42-no-threadflag but forced single-threaded
#include <goblint.h>

int g;

int main() {
  __goblint_check(g == 0); // SUCCESS (forced single-threaded)
  g = 1; // NORACE (forced single-threaded)
  __goblint_check(g == 1); // SUCCESS (forced single-threaded)
  return 0;
}

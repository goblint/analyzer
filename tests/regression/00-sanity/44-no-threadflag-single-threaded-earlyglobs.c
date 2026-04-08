// PARAM: --set ana.activated[-] threadflag --set ana.activated[-] threadid --enable exp.earlyglobs --enable exp.single-threaded
// Like 00-sanity/43-no-threadflag-single-threaded but with earlyglobs enabled
#include <goblint.h>

int g;

int main() {
  __goblint_check(g == 0); // UNKNOWN (earlyglobs)
  g = 1; // NORACE (forced single-threaded)
  __goblint_check(g == 1); // UNKNOWN (earlyglobs)
  return 0;
}

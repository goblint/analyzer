// PARAM: --set ana.activated[-] threadflag --set ana.activated[-] threadid --disable exp.earlyglobs
// Also no threadid which can tell being single-threaded
// With earlyglobs disabled: no threadflag means the same behavior
#include <goblint.h>

int g;

int main() {
  __goblint_check(g == 0); // UNKNOWN (no threadflag)
  g = 1; // RACE (no threadflag)
  __goblint_check(g == 1); // UNKNOWN (no threadflag)
  return 0;
}

// SKIP PARAM: --set ana.activated[+] apron
// NOCRASH
// Manually minimized from sv-benchmarks hardware-verification-bv/btor2c-lazyMod.mul6: https://github.com/goblint/analyzer/pull/2174#issuecomment-5973482282
#include <goblint.h>
#include <limits.h>

int main() {
  unsigned __int128 x = (unsigned __int128)1 << (128 - 1);
  unsigned __int128 y = x; // y should not be bottom
  __goblint_check(x > ULLONG_MAX);
  __goblint_check(y > ULLONG_MAX);
  __goblint_check(x == y);
  return 0;
}

//SKIP PARAM: --enable ana.int.interval --sets sem.int.signed_overflow assume_none --set ana.activated[+] apron --enable ana.autotune.enabled
// Check that autotuner respect goblint_stub attributes as hints to not track variables.
#include <goblint.h>

int main () {
  // Normally these appear only inside our stubs to prevent tracking relational information for variables which will never have interesting values associated with them
  int  x __attribute__((goblint_stub));
  int  y __attribute__((goblint_stub));

  if( x < y) {
    __goblint_check(x < y); //UNKNOWN
  }

  return 0;
}

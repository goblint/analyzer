// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
// extracted from sv-benchmarks float-newlib/double_req_bl_0210.c
#include <goblint.h>

typedef int __int32_t;

int main() {
  __int32_t jz, i;

  if (i < 0) {
    i = jz; // must invalidate apron invariant about i through typedef
    if (i >= 0) {
      __goblint_check(1); // reachable
    }
  }

  return 0;
}

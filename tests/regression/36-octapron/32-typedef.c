// SKIP PARAM: --sets ana.activated[+] octApron
// extracted from sv-benchmarks float-newlib/double_req_bl_0210.c
#include <assert.h>

typedef int __int32_t;

int main() {
  __int32_t jz, i;

  if (i < 0) {
    i = jz; // must invalidate octApron invariant about i through typedef
    if (i >= 0) {
      assert(1); // reachable
    }
  }

  return 0;
}

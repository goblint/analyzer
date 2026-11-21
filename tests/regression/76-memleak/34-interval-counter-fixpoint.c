// PARAM: --set ana.activated '["base","mallocWrapper","threadflag","memLeak"]' --set ana.base.privatization none --set ana.malloc.unique_address_count 1 --enable ana.sv-comp.functions

#include <stdlib.h>
extern unsigned short __VERIFIER_nondet_ushort(void);

// FIXPOINT: the loop head joins the first visit with a saturated counter (#2326).
int main(void) {
  unsigned short n = __VERIFIER_nondet_ushort();
  for (int i = 0; i < n; ++i) {
    void *p = calloc(1, 4);
    free(p);
  }
  return 0;
}

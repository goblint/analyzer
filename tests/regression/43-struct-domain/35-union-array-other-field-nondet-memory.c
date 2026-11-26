// PARAM: --enable ana.sv-comp.functions --enable ana.int.interval
// Like 32-union-array-other-field.c, but with __VERIFIER_nondet_memory.
// The union is written through its byte array and read through its qword array.
// Reading kept the unsigned char elements of the byte array, so a qword was assumed to be at most 255.
#include <stdlib.h>
#include <goblint.h>
extern void __VERIFIER_nondet_memory(void *mem, size_t size);

typedef union { unsigned long long qwords[4]; unsigned char bytes[32]; } uint256_t;
struct G { uint256_t nonce; };

int main() {
  struct G *g = malloc(sizeof(struct G));
  // Without writing the bytes first, they stay uninitialized after __VERIFIER_nondet_memory, and the bug does not show.
  for (int i = 0; i < 32; i++)
    g->nonce.bytes[i] = 0;
  __VERIFIER_nondet_memory(&g->nonce.bytes, sizeof(g->nonce.bytes));
  unsigned long long q = g->nonce.qwords[1];
  __goblint_check(q <= 255); // UNKNOWN!
  return 0;
}

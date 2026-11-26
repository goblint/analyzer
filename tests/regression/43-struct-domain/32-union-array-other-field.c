// PARAM: --enable ana.sv-comp.functions --enable ana.int.interval
// Reduced from SV-COMP intel-tdx-module tasks.
// The union is written through its byte array and read through its qword array.
// Reading kept the unsigned char elements of the byte array, so a qword was assumed to be at most 255.
// With affeq activated, the EvalInt answers of base and affeq then had incompatible ikinds, which crashed.
#include <stdlib.h>
#include <goblint.h>
extern unsigned char __VERIFIER_nondet_uchar(void);

typedef union { unsigned long long qwords[4]; unsigned char bytes[32]; } uint256_t;
struct G { uint256_t nonce; };

void nondet_bytes(unsigned char (*dest)[], int dim0) {
  for (int i = 0; i < dim0; i++)
    (*dest)[i] = __VERIFIER_nondet_uchar();
}

int main() {
  struct G *g = malloc(sizeof(struct G));
  struct G *t = malloc(sizeof(struct G));
  nondet_bytes(&g->nonce.bytes, 32);
  for (unsigned int i = 0; i < 4; i++)
    t->nonce.qwords[i] = g->nonce.qwords[i];
  unsigned long long q = g->nonce.qwords[1];
  __goblint_check(q <= 255); // UNKNOWN!
  return 0;
}

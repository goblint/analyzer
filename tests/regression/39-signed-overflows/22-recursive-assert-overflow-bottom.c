// PARAM: --enable ana.sv-comp.enabled --enable ana.sv-comp.functions --enable ana.int.def_exc --set sem.int.signed_overflow "assume_none"
// Reduced with creduce from sv-benchmarks/c/recursified_nla-digbench/recursified_ps6.c

#include <assert.h>
extern void abort(void);
void reach_error() { assert(0); }
// void __VERIFIER_assert(int cond) { if(!(cond)) { ERROR: {reach_error();abort();} } }

void a(int *b) {
    int *d = b;
    __VERIFIER_assert(6 - *d * *d * *d) ;
    *d = *d + 1;
    a(d);
}

int c = 0;

void main() {
    a(&c);
}

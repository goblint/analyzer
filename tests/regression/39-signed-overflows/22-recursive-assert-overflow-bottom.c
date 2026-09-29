// PARAM: --enable ana.sv-comp.enabled --enable ana.sv-comp.functions --enable ana.int.def_exc --set sem.int.signed_overflow "assume_none"
// Reduced with creduce from sv-benchmarks/c/recursified_nla-digbench/recursified_ps6.c

void a(int *b) {
    int *d = b;
    __goblint_assume(6 - *d * *d * *d) ;
    *d = *d + 1;
    a(d);
}

int c = 1290;

void main() {
    a(&c);
}

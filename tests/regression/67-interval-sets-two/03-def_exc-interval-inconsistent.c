// PARAM: --enable ana.int.def_exc --enable ana.int.interval_set --enable ana.sv-comp.functions --set sem.int.signed_overflow assume_none --set ana.int.refinement never
// used to crash in branch when is_bool returned true, but to_bool returned None on (0,[1,1])
// manually minimized from sv-benchmarks/c/recursive/MultCommutative-2.c
extern int __VERIFIER_nondet_int(void);

void f(int m) {
    if (m < 0) {
        f(-m);
    }
    if (m == 0) {
        return;
    }
    f(m - 1);
}

int main() {
    int m = __VERIFIER_nondet_int();
    if (m < 0 || m > 1) {
        return 0;
    }
    f(m); // m=[0,1]
    return 0;
}

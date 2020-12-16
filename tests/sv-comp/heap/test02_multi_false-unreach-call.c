extern void abort(void);
void reach_error(){}
extern int __VERIFIER_nondet_int();


int main() {
    int a, b, c;
    int *p1 = __VERIFIER_nondet_int() ? &a : &c;
    int *p2 = __VERIFIER_nondet_int() ? &b : &c;

    if (p1 == p2) {
        goto ERROR;
    }

    return 0;

    ERROR: {reach_error();abort();}
    return 1;
}

extern void __VERIFIER_error() __attribute__ ((__noreturn__));
extern int __VERIFIER_nondet_int();
void __VERIFIER_assert(int cond) {
    if (!(cond)) {
        ERROR: __VERIFIER_error();
    }
    return;
}

int main()
{
    int a, b, c, d, e, f, g, h;
    __VERIFIER_assert(((a || b) && (c || d)) || ((e || f) && (g || h)));
    return 0;
}

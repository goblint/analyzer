extern void __VERIFIER_error() __attribute__ ((__noreturn__));
extern int __VERIFIER_nondet_int();
void __VERIFIER_assert(int cond) {
    if (!(cond)) {
        ERROR: __VERIFIER_error();
    }
    return;
}

int lt(int x, int y)
{
    return x < y;
}

int main()
{
    int x = __VERIFIER_nondet_int();
    // __VERIFIER_assert(x < 1000);

    int b = lt(x, 1000);
    __VERIFIER_assert(b);
    return 0;
}
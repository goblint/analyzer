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
    int x = __VERIFIER_nondet_int();
    int y = x > 0 && x < 0;
    if (y)
        __VERIFIER_error();
    return 0;
}
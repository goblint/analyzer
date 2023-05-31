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
    while (1)
    {
        int x = __VERIFIER_nondet_int();
        int y = x;
        __VERIFIER_assert(x == y);
    }
    return 0;
}
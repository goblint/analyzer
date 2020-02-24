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
    if (x < 1 || x > 2 || x > 1)
    {
    }
    else
        __VERIFIER_assert(x == 1);
    return 0;
}
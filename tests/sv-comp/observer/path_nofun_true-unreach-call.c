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
    int x, y;
    if (__VERIFIER_nondet_int())
    {
        x = 0;
        y = 1;
    }
    else
    {
        x = 1;
        y = 0;
    }

    // if (!(x + y == 1))
    if (x + y != 1)
        __VERIFIER_error();
    return 0;
}
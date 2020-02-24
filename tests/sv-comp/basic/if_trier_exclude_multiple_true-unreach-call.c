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
        if (x == 0)
        {
            __VERIFIER_assert(x == 0);
        }
        else if (x == 2)
        {
            __VERIFIER_assert(x == 2);
        }
        else
        {
            __VERIFIER_assert(x != 0 && x != 2);
        }
    }
    return 0;
}
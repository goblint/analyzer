extern void __VERIFIER_error() __attribute__ ((__noreturn__));
extern int __VERIFIER_nondet_int();

int main()
{
    while (1)
    {
        int x = __VERIFIER_nondet_int() % 100;
        if (x >= 150) // goblint is bad at %, exactly 100 should work but doesn't
            __VERIFIER_error();
    }
    return 0;
}
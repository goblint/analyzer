extern void __VERIFIER_error() __attribute__ ((__noreturn__));
extern int __VERIFIER_nondet_int();

int main()
{
    while (1)
    {
        int x = __VERIFIER_nondet_int();
        if (x)
            __VERIFIER_error();
    }
    return 0;
}
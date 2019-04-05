extern void __VERIFIER_error() __attribute__ ((__noreturn__));
// void __VERIFIER_error() { abort(); }

// extern int __VERIFIER_nondet_int();
int __VERIFIER_nondet_int() { int val; return val; }

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
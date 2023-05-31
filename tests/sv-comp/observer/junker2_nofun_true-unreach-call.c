extern void __VERIFIER_error() __attribute__ ((__noreturn__));

int main()
{
    int x;
    int p = 1; // malloc
    for (x = 10; x > 0; x--)
    {
        if (p != 1) // check against use-after-free
            __VERIFIER_error();
        if (x == 1)
        {
            if (p != 1) // check against double-free
                __VERIFIER_error();
            p = 0; // free
        }
    }
    if (p != 0) // check against no-free
        __VERIFIER_error();
    return 0;
}
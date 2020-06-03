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
            p = 0; // free
    }
    return 0;
}
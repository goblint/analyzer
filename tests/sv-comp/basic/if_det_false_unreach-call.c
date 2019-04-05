extern void __VERIFIER_error() __attribute__ ((__noreturn__));
// void __VERIFIER_error() { abort(); }

int main()
{
    while (1)
    {
        int x = 1;
        if (x)
            __VERIFIER_error();
    }
    return 0;
}
extern void __VERIFIER_error() __attribute__ ((__noreturn__));
void __VERIFIER_assert(int cond) {
    if (!(cond)) {
        ERROR: __VERIFIER_error();
    }
    return;
}

int main()
{
    int x;
    int p = 1; // malloc
    for (x = 10; x > 0; x--)
    {
        __VERIFIER_assert(p == 1); // check against use-after-free
        if (x == 1)
            p = 0; // free
    }
    return 0;
}
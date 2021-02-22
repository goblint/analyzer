extern void __VERIFIER_error() __attribute__ ((__noreturn__));
extern int __VERIFIER_nondet_int();

void foo()
{
    __VERIFIER_error();
}

int main()
{
    void *p = 0;
    int x = __VERIFIER_nondet_int() % 2; // TODO: Goblint can't handle (unsigned) __VERIFIER_nondet_uint() % 2
    if (x == 0 || x == 1 || x == -1)
        p = malloc(1);
    else
        p = &foo; // actually dead

    free(p); // TODO: free shouldn't spawn
    return 0;
}
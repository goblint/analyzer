extern void __VERIFIER_error() __attribute__ ((__noreturn__));
extern int __VERIFIER_nondet_int();

void foo()
{
    __VERIFIER_error();
}

int main()
{
    void (*p)() = &foo;
    free(p); // TODO: free shouldn't spawn
    return 0;
}
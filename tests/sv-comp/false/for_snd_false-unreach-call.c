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
    int i; // Ultimate can't handle i declared in for
    // Ultimate outputs both iterations..., can do better?
    for (i = 0; i < 10; i++)
    {
        // assumption: i == 1?
        __VERIFIER_assert(i == 0);
    }
    return 0;
}
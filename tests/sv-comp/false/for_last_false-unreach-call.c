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
    // Ultimate outputs all iterations..., can do better?
    for (i = 0; i < 10; i++)
    {
        // assumption: i == 9?
        __VERIFIER_assert(i < 9);
    }
    return 0;
}
extern void __VERIFIER_error() __attribute__ ((__noreturn__));
extern int __VERIFIER_nondet_int();
void __VERIFIER_assert(int cond) {
    if (!(cond)) {
        ERROR: __VERIFIER_error();
    }
    return;
}

void fun()
{
    int i; // Ultimate can't handle i declared in for
    for (i = 1; i < 1000; i += 2)
    {
        __VERIFIER_assert(i < 2000);
    }
    // Ultimate benefits from cycle invariant: i % 2 == 1
    __VERIFIER_assert(i == 1001);
}

int main()
{
    fun();
    return 0;
}
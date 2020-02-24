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
    for (i = 0; i < 1000; i++)
    {
        __VERIFIER_assert(i < 2000);
    }
}

int main()
{
    fun();
    return 0;
}
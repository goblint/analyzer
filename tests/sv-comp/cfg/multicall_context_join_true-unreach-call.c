extern void __VERIFIER_error() __attribute__ ((__noreturn__));
extern int __VERIFIER_nondet_int();
void __VERIFIER_assert(int cond) {
    if (!(cond)) {
        ERROR: __VERIFIER_error();
    }
    return;
}

void foo(int x)
{
    __VERIFIER_assert(x - 1 < x);
}

int main()
{
    if (__VERIFIER_nondet_int())
        foo(1);
    else
        foo(2);
    return 0;
}
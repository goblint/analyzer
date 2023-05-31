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
    foo(1);
    foo(2);
    return 0;
}
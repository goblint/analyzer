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

void bar(int x)
{
    __VERIFIER_assert(x < x + 1);
}

int main()
{
    void (*fun)(int);
    if (__VERIFIER_nondet_int())
        fun = &foo;
    else
        fun = &bar;

    (*fun)(1);
    return 0;
}
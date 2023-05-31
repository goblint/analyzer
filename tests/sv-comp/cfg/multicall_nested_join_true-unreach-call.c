extern void __VERIFIER_error() __attribute__ ((__noreturn__));
extern int __VERIFIER_nondet_int();
void __VERIFIER_assert(int cond) {
    if (!(cond)) {
        ERROR: __VERIFIER_error();
    }
    return;
}

void foo(int x, int y)
{
    __VERIFIER_assert(x < y);
}

void bar(int x)
{
    if (__VERIFIER_nondet_int())
        foo(x, 3);
    else
        foo(x, 4);
}

int main()
{
    if (__VERIFIER_nondet_int())
        bar(1);
    else
        bar(2);
    return 0;
}
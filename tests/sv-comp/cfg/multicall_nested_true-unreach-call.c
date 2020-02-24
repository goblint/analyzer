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
    foo(x, 3);
    foo(x, 4);
}

int main()
{
    bar(1);
    bar(2);
    return 0;
}
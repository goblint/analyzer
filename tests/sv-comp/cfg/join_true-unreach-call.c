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
    int x;
    if (__VERIFIER_nondet_int())
        x = 1;
    else
        x = 2;
    __VERIFIER_assert(1 <= x && x <= 2);
    return 0;
}
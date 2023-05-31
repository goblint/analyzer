extern void __VERIFIER_error() __attribute__ ((__noreturn__));
void __VERIFIER_assert(int cond) {
    if (!(cond)) {
        ERROR: __VERIFIER_error();
    }
    return;
}

int g = 1;

int main()
{
    __VERIFIER_assert(g == 1);
    return 0;
}
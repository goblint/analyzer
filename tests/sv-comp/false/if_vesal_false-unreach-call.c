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
    int x = 1;
    int y = __VERIFIER_nondet_int();
    int z = __VERIFIER_nondet_int();

    // assumption: z > 5 or z == 6
    // control: condition-true
    if (z > 5)
        // assumption: y == 0
        x = y;
    else
        x++;

    __VERIFIER_assert(x != 0);
    return 0;
}
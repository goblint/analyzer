// --set ana.activated[+] observer --set ana.path_sens[+] observer

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
    int x, y, z;
    if (__VERIFIER_nondet_int())
    {
        x = 0;
        y = 1; // to be killed in observer
    }
    else
    {
        x = 1;
    }
    z = 1;

    __VERIFIER_assert(x == 1);
    return 0;
}
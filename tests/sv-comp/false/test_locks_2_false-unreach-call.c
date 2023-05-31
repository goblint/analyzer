extern void __VERIFIER_error() __attribute__ ((__noreturn__));
extern int __VERIFIER_nondet_int();

int main()
{
    int p1 = __VERIFIER_nondet_int();  // condition variable
    int lk1; // lock variable

    int p2 = __VERIFIER_nondet_int();  // condition variable
    // assumption: p2 == 0
    int lk2; // lock variable


    int cond;

    while(1) {
        cond = __VERIFIER_nondet_int();
        // assumption: cond != 0 or cond == 1
        // control: condition-false
        if (cond == 0) {
            goto out;
        } else {}
        lk1 = 0; // initially lock is open

        lk2 = 0; // initially lock is open

    // lock phase
        if (p1 != 0) {
            lk1 = 1; // acquire lock
        } else {}

        if (p2 != 0) {
            lk2 = 1; // acquire lock
        } else {}

    // unlock phase
        if (p1 != 0) {
            if (lk1 != 1) goto ERROR; // assertion failure
            lk1 = 0;
        } else {}

        // control: condition-false
        if (p2 != 0) {
            if (lk2 != 1) goto ERROR; // assertion failure
            lk2 = 0;
        } else {goto ERROR;}

    }
  out:
    return 0;
  ERROR: __VERIFIER_error();
    return 0;
}

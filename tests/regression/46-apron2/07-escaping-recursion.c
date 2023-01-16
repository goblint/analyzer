// SKIP PARAM: --set ana.activated[+] apron --set ana.relation.privatization top
// Copy of 01/52 for Apron
#include <goblint.h>

int rec(int i,int* ptr) {
    int top;
    int x = 17;

    if(i == 0) {
        rec(5,&x);
        // Recursive call may have modified x
        __goblint_check(x == 17); //UNKNOWN!

        // If we analyse this with int contexts, there is no other x that is reachable, so this
        // update is strong
        x = 17;
        __goblint_check(x == 17);
    } else {
        x = 31;

        // ptr points to the outer x, it is unaffected by this assignment
        // and should be 17
        __goblint_check(*ptr == 31); //UNKNOWN!

        if(top) {
            ptr = &x;
        }

        // ptr may now point to both the inner and the outer x
        *ptr = 12;
        __goblint_check(*ptr == 12); //UNKNOWN!
        __goblint_check(x == 12); //UNKNOWN!

        if(*ptr == 12) {
            __goblint_check(x == 12); //UNKNOWN!
        }

        // ptr may still point to the outer instance
        __goblint_check(ptr == &x); //UNKNOWN!

        // Another copy of x is reachable, so we are conservative and do a weak update
        x = 31;
        __goblint_check(x == 31); // UNKNOWN
    }
    return 0;
}


int main() {
    int t;
    rec(0,&t);
    return 0;
}

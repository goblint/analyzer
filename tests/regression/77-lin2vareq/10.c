//SKIP PARAM: --set ana.activated[+] lin2vareq  --set sem.int.signed_overflow assume_none --enable ana.int.interval
#include <stdio.h>

int undefinedFunction();

int main() {
    int x;

    x = undefinedFunction();

    __goblint_check(x == undefinedFunction()); //UNKNOWN!

    return 0;
}

//This test case sets a variable to a function that was not defined
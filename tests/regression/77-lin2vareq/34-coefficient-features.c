//SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none
// this test checks basic coefficient handing in main and join capabilities in loop

#include <goblint.h>

void loop() {
    int random;
    int i = 0;
    int x = 0;
    int y = 0;

    x=x+4;
    y=y+8;
    i=i+1;

    if (random) {
        x=x+4;
        y=y+8;
        i=i+1;
    }

    __goblint_check(x == 4*i); //SUCCESS

    for(i = 1; i < 100; i++) {
        x=x+4;
        y=y+8;

        __goblint_check(y == 2*x); //SUCCESS
    }

    x=0;
    y=0;

    for(i = 1; i < 100; i++) {
        x=x+4;
        y=y+8;

        __goblint_check(y == 2*x); //SUCCESS
        __goblint_check(x == 4*i); //SUCCESS
    }
}

void main() {
    int a;
    int b;
    int c;
    int unknown;
    a = 4;

    b = 4*c;

    __goblint_check(b == 4*c); //SUCCESS

    b = a*c;

    __goblint_check(b == 4*c); //SUCCESS

    if (5*b == 20*unknown + a){

        __goblint_check(5*b == 20*unknown + a); //SUCCESS
    }

    b = unknown ? a*c : 4*c;

    __goblint_check(b == 4*c); //SUCCESS

    loop();

}
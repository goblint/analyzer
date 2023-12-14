//SKIP PARAM: --set ana.activated[+] lin2vareq
#include <stdio.h>

int main() {
    int x;
    int y = 5; 

    int result1 = x + y;
    int result2 = y + x;

    __goblint_check(result1 == result2); //SUCCESS

    return 0;
}

//This test case includes variable with unknown values

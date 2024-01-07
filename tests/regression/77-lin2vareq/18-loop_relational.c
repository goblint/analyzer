//SKIP PARAM: --set ana.activated[+] lin2vareq
#include <stdio.h>
#include <goblint.h>

int main() {
    int x = 0;
    int y = 10;
    int z = 5;

    for(int i = 0; i < 3; i++) {
        x += z; 
        y -= i;  
        z += 2; 
    }

    __goblint_check(x == 21);  //UNKNOWN!
    __goblint_check(y == 7);   //UNKNOWN!
    __goblint_check(z == 11);  //UNKNOWN!
    return 0;
}

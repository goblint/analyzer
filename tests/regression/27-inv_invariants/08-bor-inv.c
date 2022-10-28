#include <goblint.h>

int main(){


    int x = 2;
    int y = 3;
    int z = 0;
    int n = 0;
    int nn = 0;
    int top;

    if( x + y == 4){
        z = 3;
    }
    __goblint_check(z == 0);

    // if( ((n - nn) == 3) ==  (x+top == 3) * (y+ top == 3)){
    //     z = 1;
    // } else {
    //     z = 3;
    // }

    // __goblint_check(z == 1);
    return 0;
}

#include <goblint.h>

int main(void) {
    int x;
    __goblint_assume(x != __INT32_MAX__);
    int one = 1;
    int two = 2;
    if(x + one == two){
        __goblint_check(x == one); // SUCC
    }
}
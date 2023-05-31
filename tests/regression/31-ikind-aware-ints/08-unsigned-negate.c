#include <goblint.h>
#include <stdio.h>

int main(){
    unsigned int x  = 1;

    unsigned int y = -x;

    __goblint_check(y == 4294967295);
    printf("y: %u\n", y);
    return 0;
}

#include <stdlib.h>
#include <assert.h>

int main() {
    unsigned char a = 200;

    unsigned char* y = malloc(1);
    *y = a;

    signed char *z = y;
    __goblint_check(*z == -56);

    // Two
    signed char* s = malloc(10*sizeof(signed char));
    s[0] = -5;

    unsigned char* us = s;
    *us = 12;
}
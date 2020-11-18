#include <stdlib.h>
#include <assert.h>

int main() {
    unsigned char a = 200;

    unsigned char* y = malloc(1);
    *y = a;

    signed char *z = y;
    assert(*z == -56);
}
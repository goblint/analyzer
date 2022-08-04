#include <assert.h>

int main() {
    unsigned char a = 200;

    signed char x;
    unsigned char* y = &x;
    *y = a;

    __goblint_check(x == -56);
}
#include <assert.h>

int main() {
    unsigned char a = 200;

    signed char x;
    unsigned char* y = &x;
    *y = a;

    assert(x == -56);
}
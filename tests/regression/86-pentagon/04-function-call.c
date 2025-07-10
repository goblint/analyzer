#include <goblint.h>

int main(void) {
    int x = -1;
    int y = 42;
    double z = 0;
    x = f(x, y, z);
    __goblint_check(x == 48); // SUCC
}

int f(int x, int y, double z) {
    return 48;
}
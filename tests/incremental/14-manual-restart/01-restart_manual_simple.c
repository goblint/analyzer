#include <assert.h>

int g = 4;

int main() {
    int x = g;
    __goblint_check(x == 4);
    return 0;
}

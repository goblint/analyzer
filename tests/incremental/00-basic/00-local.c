#include <assert.h>

int main() {
    int x = 1;
    __goblint_check(x == 1); // success before, success after
    return 0;
}
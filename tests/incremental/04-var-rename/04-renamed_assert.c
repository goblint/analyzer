#include <goblint.h>

// local var used in assert is renamed (no semantic changes)
int main() {
    int myVar = 0;

    __goblint_check(myVar < 11);

    return 0;
}

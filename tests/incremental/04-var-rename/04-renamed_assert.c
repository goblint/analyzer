#include <goblint.h>

int main() {
    int myVar = 0;

    __goblint_check(myVar < 11);

    return 0;
}
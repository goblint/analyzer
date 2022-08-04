#include <assert.h>

int main() {
    int varFirstIteration = 0;

    varFirstIteration++;

    __goblint_check(varFirstIteration < 10);
    return 0;
}

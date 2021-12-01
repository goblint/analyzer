#include <assert.h>

int g = 1;

int main() {
    // After the presolve phase, g is in the start state but neither in the context nor in the start variables.
    // If the start state of main is not overwriten when main is destabilized because of a changed start state,
    // the first assert will wrongly succeed and the second fail in the incremental run
    assert(g == 1); // SUCCESS
    assert(g == 2); // FAIL
    return 0;
}

// The patch for this test case is empty on purpose. The problem described does occur in the incremental run even without any changes.
#include <goblint.h>

int g = 1;

void f() {
    g = 2;
}

int main() {
    f();
    __goblint_check(g == 2); // UNKNOWN!
    __goblint_check(g == 1); // UNKNOWN! (when wrongly overriding the start state of start functions this did succeed in the incremental run)
    return 0;
}

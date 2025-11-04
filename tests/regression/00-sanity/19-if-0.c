#include <goblint.h>

void stuff() {

}

int main() {
    // MyCFG shouldn't ignore loops with exp 0 because they may have else branches
    if (0) {
        foo: // label prevents CIL from optimizing away this branch
        stuff(); // something non-empty
    }
    else {
        // realnode finds this as immediate successor of entry, so problem is avoided
        __goblint_check(1); // must be reachable
    }
    return 0;
}
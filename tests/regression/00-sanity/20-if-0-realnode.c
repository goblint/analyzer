#include <assert.h>

void stuff() {

}

int main() {
    // MyCFG shouldn't ignore loops with exp 0 because they may have else branches
    bar:
    if (0) {
        foo: // label prevents CIL from optimizing away this branch
        stuff(); // something non-empty
    }
    else {
        goto bar; // direct realnode to If via goto
    }
    return 0;
}
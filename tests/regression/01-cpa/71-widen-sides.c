// PARAM: --set ana.ctx_insens "['base', 'mallocWrapper']"  --enable ana.int.interval --sets solvers.td3.side_widen sides-local
#include <goblint.h>

int further(int n) {
    // Even sides-local can not save us here :(
    __goblint_check(n <= 1); //TODO
}


int fun(int n, const char* arg) {
    // Fails with solvers.td3.side_widen sides, needs sides-local
    __goblint_check(n <= 1);
    further(n);
}

void doIt(char* const arg) {
    // These calls cause side-effects to the start state of [fun]
    // As the calls happen after each other, we have two increasing contributions to [fun] from this unknown
    // In the setting with solvers.td3.side_widen sides, [fun] thus becomes a wpoint
    fun(0, arg);
}


int main() {
    doIt("one");
    doIt("two");

    // In the setting with solvers.td3.side_widen sides, widening happens and the bound is lost
    fun(1, "org");
}

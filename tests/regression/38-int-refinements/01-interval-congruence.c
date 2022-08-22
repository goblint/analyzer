// PARAM: --disable ana.int.def_exc --enable ana.int.interval --enable ana.int.congruence --set ana.int.refinement fixpoint
#include <assert.h>

int main(){
    int r;

    if (r) {
        r = 2;
    } else {
        r = 7;
    }

    // At this point r in the congr. dom should be 2 + 5Z
    int k = r;
    if (k >= 3) {

        // After refinement with congruences, the lower bound should be 7 as the numbers 3 - 6 are not in the congr. class
        __goblint_check(k < 7); // FAIL
    }

    int l;
    if (l) {
        l = 37;
    } else {
        l = 42;
    }

    if (l <= 41) {
        // Similarly to before, the upper bound should be 37 now.
        __goblint_check(l > 37); // FAIL
    }
    return 0;
}

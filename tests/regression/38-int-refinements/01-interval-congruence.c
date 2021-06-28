// PARAM: --disable ana.int.def_exc --enable ana.int.interval --enable ana.int.congruence --enable ana.int.congruence_no_overflow --enable ana.int.refinement
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
        // TODO fix refinement and uncomment the following line
        //assert (k < 7); // FAIL
    }

    //if (r >= -11 && r <= -4) {
    //    assert (r == -8);
    //}
    return 0;
}

// PARAM: --enable ana.int.congruence --set sem.int.signed_overflow assume_none
#include <goblint.h>

int main() {
    int top;
    int i = 0;
    if(top % 17 == 3) {
        __goblint_check(top%17 ==3); //TODO (Refine top to be positive above, and reuse information in %)
        if(top %17 != 3) {
            i = 12;
        } else {

        }
    }
    __goblint_check(i ==0); // TODO
    i = 0;

    if(top % 17 == 0) {
        __goblint_check(top%17 == 0);
        if(top %17 != 0) {
            i = 12;
        }
    }
    __goblint_check(i == 0);
}

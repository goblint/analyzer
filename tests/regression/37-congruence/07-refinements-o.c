// PARAM: --enable ana.int.congruence
#include <assert.h>

void unsignedCase() {
    unsigned int top;
    unsigned int i = 0;
    if(top % 17 == 3) {
        __goblint_check(top%17 ==3);
        if(top %17 != 3) {
            i = 12;
        } else {

        }
    }
    __goblint_check(i ==0);

    if(top % 17 == 0) {
        __goblint_check(top%17 == 0);
        if(top %17 != 0) {
            i = 12;
        }
    }
    __goblint_check(i == 0);

    if(top % 3 == 17) {
        // This is unreachable in the concrete!
        __goblint_check(top%17 == 3); //UNKNOWN!
    }
}

int main() {
    int top;
    int i = 0;
    if(top % 17 == 3) {
        __goblint_check(top%17 ==3);
        if(top %17 != 3) {
            i = 12;
        } else {

        }
    }
    __goblint_check(i ==0);

    if(top % 17 == 0) {
        __goblint_check(top%17 == 0);
        if(top %17 != 0) {
            i = 12;
        }
    }
    __goblint_check(i == 0);

    if(top % 3 == 17) {
        // This is unreachable in the concrete!
        __goblint_check(top%17 == 3); //UNKNOWN!
    }

    unsignedCase();
}

//PARAM: --disable ana.int.interval --set sem.int.signed_overflow assume_wraparound
#include <assert.h>

int main(){
    int a = 0;

    // maximum value for long long
    signed long long s = 9223372036854775807;
    __goblint_check(s > 9223372036854775806);

    signed long long t = s + 2;
    // Signed overflow - The following assertion only works with sem.int.signed_overflow set to assume_wraparound
    __goblint_check(t == -9223372036854775807);

    return 0;
}

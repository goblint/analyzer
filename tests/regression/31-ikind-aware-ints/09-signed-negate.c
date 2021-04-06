//PARAM: --disable ana.int.interval --enable ana.int.wrap_on_signed_overflow
#include <assert.h>

int main(){
    int a = 0;

    // maximum value for long long
    signed long long s = 9223372036854775807;
    assert(s > 9223372036854775806);

    signed long long t = s + 2;
    // Signed overflow - The following assertion only works with ana.int.wrap_on_signed_overflow enabled
    assert(t == -9223372036854775807);

    return 0;
}

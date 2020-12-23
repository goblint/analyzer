//PARAM: --disable ana.int.interval
#include <assert.h>

int main(){
    int a = 0;

    // maximum value for ulonglong
    unsigned long long x = 18446744073709551615ull;
    if(x > 18446744073709551612ull){
        a = 1;
    }
    assert(a);

    unsigned long long y = x + 4;
    // Unsigned overflow -- The following assertion should succeed
    assert(y == 3);

    // maximum value for long long
    signed long long s = 9223372036854775807;
    assert(s > 9223372036854775806);

    signed long long t = s + 2;
    // Signed overflow -- The following assertion must be UNKNOWN!
    assert(t == -9223372036854775807); // UNKNOWN!

    return 0;
}

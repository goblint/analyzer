//PARAM: --enable ana.int.interval --disable ana.int.def_exc --disable ana.int.enums
#include <assert.h>

int main(){
    int a = 0;

    // maximum value for ulonglong
    unsigned long long x = 18446744073709551615ull;
    if(x > 18446744073709551612ull){
        a = 1;
    }
    __goblint_check(a);

    unsigned long long y = x + 4;
    __goblint_check(y == 3);

    // maximum value for long long
    signed long long s = 9223372036854775807;
    __goblint_check(s > 9223372036854775806);

    signed long long t = s + 2;
    // Signed overflow -- The following assertion must be UNKNOWN!
    __goblint_check(t == -9223372036854775807); // UNKNOWN!

    return 0;
}

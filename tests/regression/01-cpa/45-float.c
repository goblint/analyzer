// PARAM: --enable ana.int.interval --enable ana.int.def_exc --enable ana.sv-comp.functions --set ana.activated[+] var_eq --set ana.activated[+] region
#include <assert.h>

int isNan(float arg) {
    float x;
    return arg != arg;
}

int main(){
    struct blub { float f; } s;
    float fs[3];

    float top;
    // float may be NaN here, therefore the comaprison should be unknown
    __goblint_check(top == top); //UNKNOWN!
    __goblint_check(s.f == s.f); //UNKNOWN!
    __goblint_check(fs[1] == fs[1]); //UNKNOWN!

    int r = isNan(top);

    if(r) {
        __goblint_check(1);
    } else {
        __goblint_check(1);
    }
 }

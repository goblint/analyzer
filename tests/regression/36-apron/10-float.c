// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
// Copied from 01/45 for apron.
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
    assert(top == top); //UNKNOWN!
    assert(s.f == s.f); //UNKNOWN!
    assert(fs[1] == fs[1]); //UNKNOWN!

    int r = isNan(top);

    if(r) {
        assert(1);
    } else {
        assert(1);
    }
 }

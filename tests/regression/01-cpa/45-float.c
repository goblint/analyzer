// PARAM: --enable ana.int.interval --enable ana.int.def_exc --enable ana.sv-comp.functions --set ana.activated "['base','mallocWrapper','var_eq','region','expRelation']"
#include <assert.h>

int main(){
    struct blub { float f; } s;
    float fs[3];

    float top;
    // float may be NaN here, therefore the comaprison should be unknown
    assert(top == top); //UNKNOWN!
    assert(s.f == s.f); //UNKNOWN!
    assert(fs[1] == fs[1]); //UNKNOWN!
}

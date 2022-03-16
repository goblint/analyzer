//PARAM: --disable ana.int.def_exc --enable ana.int.interval
#include <assert.h>

int main() {
    int x;

    if(!x) {
    } else {
        assert(x==1); //UNKNOWN!
    }
}

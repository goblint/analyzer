// PARAM: --set sem.int.signed_overflow assume_none
#include <assert.h>

int main(void) {
    int a;

    if(a != -1) {
        int s = a+1;
        assert(s != 0);
    }
}

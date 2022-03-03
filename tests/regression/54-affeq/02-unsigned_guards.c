//SKIP PARAM: --set ana.activated[+] affeq --enable ana.int.interval --set sem.int.signed_overflow assume_none
#include <assert.h>
#include <stdio.h>

int main(){
    unsigned int i;
    int k = 0;

    if(i *  1073741824u == 3221225472u){
        printf("%u\n", i);
        assert(i == 3); // UNKNOWN!
    }

    unsigned int i;
    if (i - 2u == 4294967295u) {
        assert (i == 4294967297); // FAIL!
    }

    int x, y;


    int f = 0;
    if (y - 1 == 2147483647) {
        f = 1;
    }
    assert (f == 0);

    if (x == 1000 * y) {
        assert(1);
    }

    unsigned int x = 8;
    if (x == 8u) {
        assert (1); // reachable
    }
    return 0;
}

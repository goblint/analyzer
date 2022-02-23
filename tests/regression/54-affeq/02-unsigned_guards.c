//SKIP PARAM: --set ana.activated[+] affeq  --enable ana.int.interval
#include <assert.h>
#include <stdio.h>

int main(){
    unsigned int i;

    if(i *  1073741824u == 3221225472u){
        printf("%u\n", i);
        assert(i == 3); // UNKNOWN!
    }

    if (i - 2u == 4294967295u) {
        assert (i == 4294967297); // FAIL!
    }

    unsigned int x = 8;
    if (x == 8u) {
        assert (1); // reachable
    }
    return 0;
}

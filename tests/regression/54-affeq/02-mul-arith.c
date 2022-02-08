//SKIP PARAM: --set ana.activated[+] affeq --enable ana.int.interval
// This test fails currently due to an erroneous overflow handling
#include <assert.h>
#include <stdio.h>

int main(){
    unsigned int i;

    unsigned int r = i * 1073741824u;
    if(i *  1073741824u == 3221225472u){
        printf("%u\n", i);
        assert(i == 3); // UNKNOWN!
    }
    return 0;
}

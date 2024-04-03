//SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none

// Adapted example from https://link.springer.com/content/pdf/10.1007/BF00268497.pdf

#include <goblint.h>

void main(void) {
    int i;
    int j;
    int k;
    if(k > 200){
        return 0;
    }
    j = k + 5;

    while (j < 100) {
        __goblint_check(j - k == 5); //SUCCESS
        j = j + 3;
        k = k + 3;
    }
    __goblint_check(j - k == 5); //SUCCESS

}

// PARAM: --enable ana.int.def_exc --enable ana.int.interval_set
#include <goblint.h>

int main(void) {
    int i = 1;
    int v = 8;

    int r = 28/v;

    while(1) {
        int zgg = 17;
        if (! (i < 28 / v)) {
            zgg = 5;
            goto while_beak;
        }

        int z = 3;
        i ++;
        z = 7;
    }





    // while(i < 28/v) {
    //     int z = 3;
    //     i++;
    //     z = 7;
    // }
  while_beak:
    __goblint_check(i == 3);



    int a = 5;
    int b = 7;

    int r = 0;

    if (a == b) {
        // Dead
        r = 1;
    } else {
        r = 2;
    }

    __goblint_check(r == 1); //FAIL
    __goblint_check(r == 2);
}

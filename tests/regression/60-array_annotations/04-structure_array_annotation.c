// PARAM: --set ana.base.arrays.unrolling-factor 4 --enable annotation.goblint_array_domain
#include <goblint.h>

void w(int z[] ){
    if (z[2] == 4) {
        z[2] = 3;
    } else {
        z[2] = 2;
    }
}

struct array {
    int arr[5] __attribute__((goblint_array_domain("unroll")));
};

int main(void) {

    struct array a = {{0,1,2,3}};

    a.arr[2] = 4;

    w(a.arr);

    if (a.arr[2] == 3)
        a.arr[2] = 5;

    w(a.arr);


    __goblint_check(2 == a.arr[2]);

}

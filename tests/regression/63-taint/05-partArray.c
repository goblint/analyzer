// PARAM: --enable ana.int.interval --set ana.base.arrays.domain partitioned --disable ana.base.context.int --set ana.activated[+] taintPartialContexts
// adapted from 22 06
#include <goblint.h>

void do_first(int* arr) {
    int x = arr[0];
    arr[0] = 3;
}

void init_array(int* arr, int val) {
    for(int i = 0; i < 20; i++) {
        arr[i] = val;
    }
    arr[0] = val;
    __goblint_check(arr[2] == val);
    __goblint_check(arr[10] == val);
}

int main() {
    int a[20];

    init_array(a, 42);
    __goblint_check(a[2] == 42);
    __goblint_check(a[10] == 42);

    do_first(a);
    __goblint_check(a[0] == 3);
}

// PARAM: --set ana.int.interval true --set ana.base.arrays.domain partitioned

#include<stdlib.h>
#include<assert.h>

struct h {
    int a[2];
    int b;
};

int main(void) {
    struct h *m = calloc(3,sizeof(int));

    struct h *a = calloc(10,sizeof(struct h));

    struct h *b = a+1;
    a->a[1] = 3;
    int* ptr = &(b->a[1]);
    __goblint_check(*ptr == 3); //UNKNOWN
}

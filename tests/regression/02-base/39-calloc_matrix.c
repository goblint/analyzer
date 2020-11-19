#include<stdlib.h>
#include<assert.h>

struct h {
    int a[2]; 
    int b;
};

int main(void) {
    int *ro = calloc(2,sizeof(int));
    ro[0] = 3;
    assert(ro[1] != 3); //UNKNOWN

    struct h *m = calloc(3,sizeof(int));

    struct h *a = calloc(10,sizeof(struct horst));

    struct h *b = a+1;
    a->a[1] = 3;
    int* ptr = &(b->a[1]);
    assert(*ptr == 3); //UNKNOWN

    int (*r)[5] = calloc(2, sizeof(int[5]));
    r[0][1] = 3;
    int* z = &r[0][1];

    assert(*z == 3); //UNKNOWN
}
// PARAM: --set ana.base.arrays.domain partitioned --enable annotation.int.enabled --set ana.int.refinement fixpoint

#include<stdlib.h>
#include<assert.h>

typedef struct {
  int x;
  int y;
} data;

data *d;
int main(void) __attribute__((goblint_precision("no-def_exc","interval","enums")));

int main(void) {
    d = calloc(1,sizeof(data));
    d -> x = 0;
    d -> y = 0;

    data e = {.x = 0, .y = 0};

    __goblint_check(d->x == e.x);
    __goblint_check(d->y == e.y);

    int a = d -> x;
    int b = d -> y;

    __goblint_check(a != 3);
    __goblint_check(b != 4);

    d -> x = 3;
    d -> y = 4;

    data f = {.x = 3, .y = 3};

    __goblint_check(d->x == f.x); //UNKNOWN
    __goblint_check(d->y == f.y); //FAIL!

    a = d -> x;
    b = d -> y;

    __goblint_check(a == 3); //UNKNOWN
    __goblint_check(b == 4); //UNKNOWN
}

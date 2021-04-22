// PARAM: --set ana.int.interval true --enable exp.partition-arrays.enabled

#include<stdlib.h>
#include<assert.h>

typedef struct {
  int x;
  int y;
} data;

data *d;

int main(void) {
    d = calloc(1,sizeof(data));
    d -> x = 0;
    d -> y = 0;

    data e = {.x = 0, .y = 0};

    assert(d->x == e.x);
    assert(d->y == e.y);

    int a = d -> x;
    int b = d -> y;

    assert(a != 3);
    assert(b != 4);

    d -> x = 3;
    d -> y = 4;

    data f = {.x = 3, .y = 3};

    assert(d->x == f.x); //UNKNOWN
    assert(d->y == f.y); //UNKNOWN

    a = d -> x;
    b = d -> y;

    assert(a == 3); //UNKNOWN
    assert(b == 4); //UNKNOWN
}
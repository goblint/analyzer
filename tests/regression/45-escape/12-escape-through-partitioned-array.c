// PARAM: --set ana.base.arrays.domain partitioned
#include <pthread.h>
#include <unistd.h>
#include <goblint.h>

// 11-escape-through-array-member.c under an array domain that keeps elements
// apart, with the pointer at an index other than the first. Reading the array
// at one constant index answers for every element under the default trivial
// domain and for one partition here, so only following each element of a
// constant-length array leaves y escaped in both.

struct fields {
    int *array[2];
};

struct fields shared;

void *foo(void *p) {
    *(shared.array[1]) = 17;
    return NULL;
}

int main() {
    int x = 0;
    int y = 0;
    struct fields replacement;
    replacement.array[0] = &x;
    replacement.array[1] = &y;

    pthread_t thread;
    pthread_create(&thread, NULL, foo, NULL);
    shared = replacement;
    sleep(3);
    __goblint_check(y == 0); // UNKNOWN!
    pthread_join(thread, NULL);
    return 0;
}

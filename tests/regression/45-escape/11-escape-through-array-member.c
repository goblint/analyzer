#include <pthread.h>
#include <unistd.h>
#include <goblint.h>

// 10-escape-through-struct-assignment.c with the pointer in an array member.
// ReachableFrom answers for an array lvalue with the object the array belongs
// to, so following the assigned struct member by member is not enough on its
// own: the array member has to be followed element by element as well.

struct fields {
    int *array[1];
};

struct fields shared;

void *foo(void *p) {
    *(shared.array[0]) = 17;
    return NULL;
}

int main() {
    int x = 0;
    struct fields replacement;
    replacement.array[0] = &x;

    pthread_t thread;
    pthread_create(&thread, NULL, foo, NULL);
    shared = replacement;
    sleep(3);
    __goblint_check(x == 0); // UNKNOWN!
    pthread_join(thread, NULL);
    return 0;
}

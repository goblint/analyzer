#include <pthread.h>
#include <unistd.h>
#include <goblint.h>

// A whole-struct assignment into a global publishes the pointers the struct
// holds, so the local x points to escapes. EscapeUtil.escaping has to follow
// the assigned value member by member to see it: asking ReachableFrom for the
// struct lvalue answers with the struct itself rather than with what its
// members point to.

struct fields {
    int *scalar;
};

struct fields shared;

void *foo(void *p) {
    *(shared.scalar) = 17;
    return NULL;
}

int main() {
    int x = 0;
    struct fields replacement;
    replacement.scalar = &x;

    pthread_t thread;
    pthread_create(&thread, NULL, foo, NULL);
    shared = replacement;
    sleep(3);
    __goblint_check(x == 0); // UNKNOWN!
    pthread_join(thread, NULL);
    return 0;
}

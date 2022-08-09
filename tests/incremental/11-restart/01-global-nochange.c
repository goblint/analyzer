#include <assert.h>
#include <pthread.h>

int g = 1;

void* t_fun(void *arg) {
    g = 2;
    return NULL;
}

int main() {
    pthread_t id;
    pthread_create(&id, NULL, t_fun, NULL); // just go multithreaded

    __goblint_check(g == 1); // UNKNOWN before and after
    __goblint_check(g == 2); // UNKNOWN before and after
    __goblint_check(g == 0); // FAIL before and after
    return 0;
}
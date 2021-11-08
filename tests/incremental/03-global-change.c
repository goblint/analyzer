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

    assert(g == 1); // unknown before, unknown after
    assert(g == 2); // unknown before, fail after
    assert(g == 0); // fail before, unknown after
    return 0;
}
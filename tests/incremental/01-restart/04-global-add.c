#include <assert.h>
#include <pthread.h>

int g = 1;

void* t_fun(void *arg) {
    return NULL;
}

int main() {
    pthread_t id;
    pthread_create(&id, NULL, t_fun, NULL); // just go multithreaded

    assert(g == 1); // success before, unknown after
    return 0;
}
// PARAM: --enable ana.int.interval
#include <pthread.h>
#include <assert.h>

int g;

void* t_fun(void *arg) {
    int x = g;
    if (x < 10) {
        x++;
        g = x;
    }
    return NULL;
}

int main() {
    pthread_t id;
    pthread_create(&id, NULL, t_fun, NULL);

    int x = g;
    assert(x <= 10);
    return 0;
}
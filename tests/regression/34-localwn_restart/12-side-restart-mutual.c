// SKIP PARAM: --enable ana.int.interval
// requires reuse of final local value for global restart
#include <pthread.h>
#include <assert.h>

int g, h;

void* t_fun(void *arg) {
    int x = g;
    if (x < 10) {
        x++;
        h = x;
    }
    return NULL;
}

void* t_fun2(void *arg) {
    int y = h;
    if (y < 10) {
        y++;
        g = y;
    }
    return NULL;
}

int main() {
    pthread_t id, id2;
    pthread_create(&id, NULL, t_fun, NULL);
    pthread_create(&id2, NULL, t_fun2, NULL);

    int x = g;
    int y = h;
    __goblint_check(x <= 10);
    __goblint_check(y <= 10);
    return 0;
}
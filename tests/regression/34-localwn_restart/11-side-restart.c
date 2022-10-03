// SKIP PARAM: --enable ana.int.interval
// TODO: requires restart of global during/after normal solving
#include <pthread.h>
#include <assert.h>

int g;

void* t_fun(void *arg) {
    int x = g;
    __goblint_check(x <= 8);
    return NULL;
}

int main() {
    pthread_t id;
    pthread_create(&id, NULL, t_fun, NULL);

    int i = 0;
    int j;

    for (j = 1; j < 10; j++) {
        for (i = 0; i < j; i++) {
            g = i;
        }
    }
    return 0;
}
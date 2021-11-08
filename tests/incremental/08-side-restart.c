#include <pthread.h>
#include <assert.h>

int g;

void* t_fun1(void *arg) {
    int x = g;
    assert(x <= 8);
    return NULL;
}

void* t_fun2(void *arg) {
    g = 0;
    return NULL;
}

int main() {
    pthread_t id1, id2;
    pthread_create(&id1, NULL, t_fun1, NULL);
    

    int i = 0;
    int j;

    for (j = 1; j < 10; j++) {
        for (i = 0; i < j; i++) {
            g = i;
        }
    }
    assert(i <= 9);

    pthread_create(&id2, NULL, t_fun2, NULL);
    return 0;
}
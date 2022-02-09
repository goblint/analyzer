#include <pthread.h>
#include <stdio.h>

int g;

void *t_fun1(void* arg) {
    g = 1;
    return NULL;
}

void *t_fun2(void* arg) {
    g = 2;
    return NULL;
}

int main() {
    int x = 3;
    pthread_t a, b;
    pthread_create(&a, NULL, t_fun1, NULL);
    pthread_create(&b, NULL, t_fun2, NULL);
    pthread_join(a, NULL);
    pthread_join(b, NULL);
    x = g;
    return 0;
}
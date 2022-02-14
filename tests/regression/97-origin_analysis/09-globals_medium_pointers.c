#include <pthread.h>
#include <stdio.h>

int* g;
int x = 3;
int y = 2;
int z = 1;

void *t_fun1(void* arg) {
    g = &z;
    return NULL;
}

void *t_fun2(void* arg) {
    g = &y;
    return NULL;
}

int main() {
    int val = x;
    pthread_t a, b;
    pthread_create(&a, NULL, t_fun1, NULL);
    pthread_create(&b, NULL, t_fun2, NULL);
    pthread_join(a, NULL);
    pthread_join(b, NULL);
    val = *g;
    return 0;
}
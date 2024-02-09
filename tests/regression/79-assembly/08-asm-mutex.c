// PARAM: --set ana.activated[+] mutexEvents --disable asm_is_nop
#include <pthread.h>

pthread_mutex_t mutex;

int main() {
    int x = 2;
    pthread_mutex_init(&mutex, NULL);
    pthread_mutex_lock(&mutex);
    asm ("nop" : "=g" (mutex), "=x" (x));
    pthread_mutex_unlock(&mutex); // WARN
    return 0;
}


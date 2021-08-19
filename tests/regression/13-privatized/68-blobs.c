#include <pthread.h>
#include <stdlib.h>

char *v;
pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

void *thread0(void *arg)
{
    v = malloc(sizeof(char));
    pthread_mutex_lock(&m);
    v[0] = 'X';
    pthread_mutex_unlock(&m);
    return 0;
}

int main(void)
{
    pthread_t t;
    pthread_create(&t, 0, thread0, 0);

    return 0;
}

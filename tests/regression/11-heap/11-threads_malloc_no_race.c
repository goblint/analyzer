// No race should be reported because thread1 and thread2 are both unique
// and work with their own allocated memory

#include <stdlib.h>
#include <pthread.h>

int *f()
{
    int *x = malloc(sizeof(int));
    return x;
}

void *thread1(void *v)
{
    int *x = f();
    (*x)++; // NORACE
}

void *thread2(void *v)
{
    int *x = f();
    (*x)++; // NORACE
}

int main(int argc, char **argv)
{
	pthread_t tid1;
	pthread_t tid2;

    pthread_create(&tid1, NULL, thread1, NULL);
    pthread_create(&tid2, NULL, thread2, NULL);

    pthread_join(tid1, NULL);
    pthread_join(tid2, NULL);
}

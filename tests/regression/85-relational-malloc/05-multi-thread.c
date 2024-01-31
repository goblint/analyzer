// PARAM: --set ana.activated[+] memOutOfBounds --set ana.activated[+] apron  --set ana.apron.domain polyhedra    --set ana.activated[+] taintPartialContexts --set ana.ctx_insens[+] threadJoins --enable ana.int.interval
#include <stdlib.h>
#include <pthread.h>

pthread_mutex_t mtx = PTHREAD_MUTEX_INITIALIZER;

int *gptr;
void *t_other(void *arg)
{
    pthread_mutex_lock(&mtx);
    // we loose all information about the relation between the ghost variables and the len after entering multithreaded context
    int tmp = *gptr;     
    pthread_mutex_unlock(&mtx);
}

int main()
{
    int len = rand();
    len %= 10;
    gptr = malloc(sizeof(int) * len);

    pthread_t thread;
    pthread_create(&thread, NULL, t_other, NULL);

    pthread_mutex_lock(&mtx);
    for (int i = 0; i < len; i++)
    {
        gptr[i] = 42;     // NOWARN
        gptr[i + 1] = 42; // WARN
        gptr[i - 1] = 42; // WARN

        int tmp = gptr[i];     // NOWARN
        int tmp = gptr[i + 1]; // WARN
        int tmp = gptr[i - 1]; // WARN
    }
    pthread_mutex_unlock(&mtx);

    pthread_join(thread, NULL);
    free(gptr);
    return 0;
}
// PARAM: --set ana.activated[+] memOutOfBounds --set ana.activated[+] apron  --set ana.apron.domain polyhedra  --set ana.activated[+] threadJoins  --set ana.path_sens[+] threadflag  --set ana.base.privatization mutex-meet-tid
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>

pthread_mutex_t mtx = PTHREAD_MUTEX_INITIALIZER;

void *t_other(int *gptr)
{
    pthread_mutex_lock(&mtx);
    int tmp = *gptr; // TODO analysis does not work with escaped/global len variable
    pthread_mutex_unlock(&mtx);
}

int main()
{
    int len = rand();
    len %= 10;
    int*  gptr = malloc(sizeof(int) * len);

    pthread_t thread;
    pthread_create(&thread, NULL, t_other, gptr);

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
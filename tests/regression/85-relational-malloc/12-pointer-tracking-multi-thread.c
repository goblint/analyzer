// PARAM: --set ana.activated[+] memOutOfBounds --set ana.activated[+] threadJoins --set ana.activated[+] apron  --set ana.apron.domain polyhedra --enable ana.int.interval --set ana.activated[+] taintPartialContexts  --set ana.activated[+] thread  --enable ana.apron.pointer_tracking --set sem.int.signed_overflow assume_none  --disable warn.integer
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>

int *gptr;

pthread_mutex_t mtx = PTHREAD_MUTEX_INITIALIZER;

void *t_other(int *len)
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
    int x = 0;
    int *p = gptr;
    while (x < len)
    {
        *p = 42;       // NOWARN
        *(p + 1) = 42; // WARN

        int tmp = *p;   // NOWARN
        tmp = *(p - 1); // WARN
        p++;
        x++;
    }
    pthread_mutex_unlock(&mtx);

    pthread_join(thread, NULL);
    free(gptr);
    return 0;
}
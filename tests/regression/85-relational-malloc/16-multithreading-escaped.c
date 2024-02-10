// SKIP PARAM: --set ana.activated[+] memOutOfBounds --set ana.activated[+] apron  --set ana.apron.domain polyhedra  --set ana.activated[+] threadJoins --enable ana.int.interval --set ana.activated[+] allocVarEscaped
#include <stdlib.h>
#include <pthread.h>

pthread_mutex_t mtx = PTHREAD_MUTEX_INITIALIZER;

// int len;
int *gptr;

void *t_other(void *arg)
{
    int len = *((int *)arg);
    pthread_mutex_lock(&mtx);
    // relational memOutOfBounds analysis doesn't work with escaped len variables,
    // beause we loose all the relational information between the ghost variable and len information
    // when we enter_multithreading in relationPriv
    for (int i = 0; i < len; i++)
    {
        gptr[i] = 42; // TODO NOWARN
    }
    pthread_mutex_unlock(&mtx);
}

int main()
{
    int len = rand();
    len %= 10;
    int *gptr = (int *)malloc(sizeof(int) * len);

    pthread_t thread;
    pthread_create(&thread, NULL, t_other, &len);

    pthread_mutex_lock(&mtx);
    for (int i = 0; i < len; i++)
    {
        gptr[i] = 42; // TODO NOWARN
    }
    pthread_mutex_unlock(&mtx);

    pthread_join(thread, NULL);

    free(gptr);
    return 0;
}
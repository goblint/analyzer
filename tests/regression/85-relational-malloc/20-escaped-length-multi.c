// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --set ana.activated[+] apron --set ana.apron.domain polyhedra --enable ana.apron.pointer_tracking  --set sem.int.signed_overflow assume_none --disable warn.integer
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>

int *gptr;

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *foo(void *ptr)
{
    pthread_mutex_lock(&mutex);
    int *i = (int *)ptr;
    int len = *i;
    int *x = malloc(*i * sizeof(int));

    x[*i - 1] = 10; // NOWARN

    if (*i > 4)
    {
        x = gptr;
    }
    else
    {
        gptr = x;
    }
    x[*i - 1] = 10; // WARN
    pthread_mutex_unlock(&mutex);
}

void *foo2(void *ptr)
{
    int *i = (int *)ptr;
    int len = *i;
    // without an mutex the ghost variable is not added to the relation
    int *x = malloc(*i * sizeof(int));

    x[*i - 1] = 10; // UNKOWN This is unknown the relation between the ghost variable is not known
}

int main()
{
    pthread_t t1, t2, t3, t4;

    int one = 5;
    int ten = 10;
    pthread_create(&t1, NULL, foo, &one);
    pthread_create(&t2, NULL, foo, &ten);

    pthread_create(&t1, NULL, foo2, &one);
    pthread_create(&t2, NULL, foo2, &ten);

    pthread_join(t1, NULL);
    pthread_join(t2, NULL);
    return 0;
}

// NOMAC PARAM: --set ana.activated[+] 'pthreadBarriers'
#include<pthread.h>
#include<stdio.h>
#include<unistd.h>
#include<goblint.h>

pthread_barrier_t *barrier;

int main(int argc, char const *argv[])
{
    int top = __VERIFIER_nondet_int();
    int i = 0;
    barrier = mmap(((void *)0), sizeof(*barrier), 0x1 | 0x2, 0x01 | 0x20, -1, 0);
    if (barrier == ((void *) -1)) {
        return 1;
    }

    pthread_barrierattr_t barattr;
    if (pthread_barrierattr_init(&barattr)) {
        return 1;
    }
    pthread_barrierattr_setpshared(&barattr, PTHREAD_PROCESS_SHARED);
    pthread_barrier_init(barrier, &barattr, 2);
    fork();

    if(top) {
        pthread_barrier_wait(barrier);
        i = 1;
    }

    __goblint_check(i == 0); //TODO
    return 0;
}

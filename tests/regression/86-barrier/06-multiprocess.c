// NOMAC PARAM: --set ana.activated[+] 'pthreadBarriers'
#include<pthread.h>
#include<stdio.h>
#include<unistd.h>
#include<goblint.h>
#include <sys/mman.h>

pthread_barrier_t *barrier;

int main(int argc, char const *argv[])
{
    int top;
    int i = 0;
    barrier = mmap(NULL, sizeof(*barrier), PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS,-1, 0);
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

    __goblint_check(i == 0); //UNKNOWN!
    return 0;
}

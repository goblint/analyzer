// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --enable ana.int.interval --enable ana.sv-comp.functions
extern int __VERIFIER_nondet_int();

#include <assert.h>
#include <pthread.h>
int global = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg)
{
    int top = __VERIFIER_nondet_int(); //rand
    pthread_mutex_lock(&mutex);
    if(top) {
        global = 5;
    } else {
        global = 12;
    }
    global = 0;
    pthread_mutex_unlock(&mutex);
}

int main(void)
{
    pthread_t t;
    pthread_create(&t, ((void *)0), t_fun, ((void *)0));

    assert(global == 0); //UNKNOWN!

    pthread_mutex_lock(&mutex);
    assert(global == 0);
    pthread_mutex_unlock(&mutex);
    return 0;
}

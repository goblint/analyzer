// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --enable ana.int.interval
extern int __VERIFIER_nondet_int();

#include<pthread.h>
#include <goblint.h>

int global = 0;

void *t_fun(void *arg)
{
    global = 5;
    __goblint_check(1);
}

int main(void)
{
    pthread_t t;
    int i = __VERIFIER_nondet_int(); //rand

    if(i < 1) {
        pthread_create(&t, ((void *)0), t_fun, ((void *)0));
        __goblint_check(global == 0); //UNKNOWN!
        i++;
    }

    __goblint_check(global == 0); //UNKNOWN!
    return 0;
}

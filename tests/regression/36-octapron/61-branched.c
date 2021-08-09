// SKIP PARAM: --sets ana.activated[+] octApron --enable ana.int.interval --sets exp.octapron.privatization mutex-meet
#include <pthread.h>
int global = 0;

void *t_fun(void *arg)
{
    global = 5;
    assert(1);
}

int main(void)
{
    pthread_t t;
    int i;

    if(i < 1) {
        pthread_create(&t, ((void *)0), t_fun, ((void *)0));
        assert(global == 0); //UNKNOWN!
        i++;
    }

    assert(global == 0); //UNKNOWN!
    return 0;
}

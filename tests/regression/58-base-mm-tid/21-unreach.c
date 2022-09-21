// PARAM: --set ana.activated[+] symb_locks --set ana.activated[+] var_eq  --set ana.base.privatization mutex-meet-tid --set ana.path_sens[+] threadflag
// This is extracted from zstd. In this example, we unsoundly claimed unreachability for the end of fun
#include<stdlib.h>
#include<pthread.h>
#include<assert.h>

pthread_mutex_t queueMutex;
int threadLimit;

static void* fun(void* arg) {
    pthread_mutex_lock(&queueMutex);

    int z = 5;
    if(threadLimit == 0) {
        z = 8;
    } else {
        z = 5;
    }

    if(threadLimit == 0) {
        z = 8;
    } else {
        z = 5;
    }

    z = 9;

    while (threadLimit == 0) {
        z = 10;
    }

    pthread_mutex_unlock(&queueMutex);
    __goblint_check(1);
}

int main() {
    pthread_t thread;
    pthread_create(&thread, NULL, &fun, NULL);
    threadLimit = 10; // RACE!

    return 0;
}

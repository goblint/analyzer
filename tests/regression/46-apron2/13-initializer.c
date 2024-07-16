// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.base.privatization none --set ana.relation.privatization mutex-meet-tid
#include <pthread.h>
#include <goblint.h>
#include <stdio.h>
#include <unistd.h>
int g, h;

void *foo(void* p){
    sleep(2);
    return NULL;
}

int main(){
    pthread_t thread;
    int y;
    g = y;
    h = y;
    __goblint_check(g == h);
    pthread_create(&thread, NULL, foo, NULL);
    __goblint_check(g == h); //TODO We would like to be able to prove that this holds (but can't as we lose g = h)
    pthread_join(thread, NULL);
    return 0;
}

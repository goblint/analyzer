// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.base.privatization none --set ana.apron.privatization mutex-meet-tid
#include <pthread.h>
#include <goblint.h>
#include <stdio.h>
#include <unistd.h>

void *foo(void* p){
    sleep(2);
    int top;
    return top;
}

int main(){
    pthread_t thread;
    int y = 8;
    pthread_create(&thread, NULL, foo, NULL);
    pthread_join(thread, &y);
    __goblint_check(y==8); //UNKNOWN!
    return 0;
}

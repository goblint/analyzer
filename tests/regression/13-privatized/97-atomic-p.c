// PARAM: --disable warn.race --enable ana.sv-comp.functions --set ana.path_sens[+] threadflag --set lib.activated[+] sv-comp --set ana.base.privatization protection-atomic
#include<pthread.h>
#include<stdlib.h>
#include<goblint.h>
extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

int g;

void fun() {
    g = 1;
}

int main(void) {
    pthread_t thread;
    pthread_create(&thread, NULL, (void*)fun, NULL);

    __VERIFIER_atomic_begin();
    g = 1;
    __VERIFIER_atomic_end();

    g = 5;
    __goblint_check(g <= 1); //UNKNOWN!


    return 0;
}

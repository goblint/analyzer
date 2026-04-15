// SKIP PARAM: --enable witness.yaml.enabled --enable ana.sv-comp.functions --set witness.yaml.validate /home/michael/Documents/goblint-cil/analyzer/tests/regression/19-ghost-witness/02-empire-detect.yml --set ana.activated[+] phaseGhost --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.relation.privatization mutex-meet-tid
// Dominik's empire thing example
#include<pthread.h>
#include<goblint.h>
extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

int x;


void fun() {
    int y = x;
    while (y> 0) {
        y--;
    }

    x++;
}

int main(void) {
    int z;
    int top;

    if(top) { x = 10000; } else { x = 1; }

    pthread_t thread;
    pthread_create(&thread, NULL, (void*)fun, NULL);

    while(z != 0) {
        z--;
    }

    x++;

    pthread_join(thread, NULL);

    top = 8;
    top = 3;

    return 0;
}

// SKIP PARAM: --set ana.activated[+] phaseGhostSplit --set ana.activated[+] threadJoins --set ana.activated[+] phaseGhost --set ana.base.privatization protection-atomic-ghost  --set witness.yaml.extraGhosts[+] ghost_a --set witness.yaml.extraGhosts[+] ghost_b --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --enable ana.int.interval
// Dominik's empire thing example
#include<pthread.h>
#include<goblint.h>
int x;

int ghost_a;
int ghost_b;

void fun() {
    int y = x;
    while (y> 0) {
        y--;
    }

    __VERIFIER_atomic_begin();
    x++;
    ghost_a = 1;
    __VERIFIER_atomic_end();

    __goblint_check(y < x); //TODO
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

    __VERIFIER_atomic_begin();
    x++;
    ghost_b = 1;
    __VERIFIER_atomic_end();

    pthread_join(thread, NULL);

    __goblint_check(x > 2); //TODO

    return 0;
}

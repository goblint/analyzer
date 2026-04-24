// PARAM: --set ana.activated[+] phaseGhostSplit --set ana.activated[+] threadJoins --set ana.activated[+] phaseGhost --set ana.base.privatization protection-atomic-ghost  --set witness.yaml.extraGhosts[+] ghost_a --set witness.yaml.extraGhosts[+] ghost_b --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --enable ana.int.interval
#include<pthread.h>
#include<goblint.h>
int x;

int ghost_a;
int ghost_b;

void fun() {
    __VERIFIER_atomic_begin();
    x++;
    ghost_a = 1;
    __VERIFIER_atomic_end();
}

int main(void) {
    int top;

    if(top) { x = 10000; }

    pthread_t thread;
    pthread_create(&thread, NULL, (void*)fun, NULL);



    __VERIFIER_atomic_begin();
    x++;
    ghost_b = 1;
    __VERIFIER_atomic_end();

    pthread_join(thread, NULL);

    __goblint_check(x >= 2);
    __goblint_check(x <= 10002);

    return 0;
}

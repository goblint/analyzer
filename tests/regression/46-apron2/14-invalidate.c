// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.base.privatization none --set ana.apron.privatization mutex-meet-tid
#include <pthread.h>
#include <goblint.h>
#include <stdio.h>
#include <unistd.h>
int g, h;

void munge(int* ptr);

int main(void) {
    int p = 5;
    munge(&p);
    __goblint_check(p == 5); //UNKNOWN!
    return 0;
}

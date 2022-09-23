<<<<<<< HEAD
// SKIP PARAM: --set solver td3 --set ana.activated "['base','threadid','threadflag','mallocWrapper','apron','escape']" --set ana.path_sens[+] threadflag --set ana.base.privatization none --set ana.relation.privatization mutex-meet-tid
=======
// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.base.privatization none --set ana.apron.privatization mutex-meet-tid
>>>>>>> master
#include <pthread.h>
#include <assert.h>
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

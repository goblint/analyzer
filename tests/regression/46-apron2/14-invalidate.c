// SKIP PARAM: --set solver td3 --set ana.activated "['base','threadid','threadflag','mallocWrapper','assert','apron','escape']" --set ana.path_sens[+] threadflag --set ana.base.privatization none --set ana.apron.privatization mutex-meet-tid
#include <pthread.h>
#include <assert.h>
#include <stdio.h>
#include <unistd.h>
int g, h;

void munge(int* ptr);

int main(void) {
    int p = 5;
    munge(&p);
    assert(p == 5); //UNKNOWN!
    return 0;
}

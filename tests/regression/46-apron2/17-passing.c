// SKIP PARAM: --set solver td3 --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.base.privatization none --set ana.apron.privatization mutex-meet-tid --set ana.base.arrays.domain partitioned
#include <pthread.h>
#include <assert.h>
#include <stdio.h>
#include <unistd.h>
struct blorg {int x; int y;};

int main(){
    struct blorg myblorg;
    int x;
    int y;
    x = y;
    fun2();
    __goblint_check(x==y);
    fun(&x);
    __goblint_check(x==y);
    fun3(&myblorg);
    __goblint_check(x==y);
    return 0;
}

int fun(int* ptr) {
    int z = 5;
}

int fun2() {
    int z = 8;
}

int fun3(struct blorg *myblorg) {
    int z = 9;
}

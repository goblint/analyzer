<<<<<<< HEAD
// SKIP PARAM: --set solver td3 --set ana.activated "['base','threadid','threadflag','mallocWrapper','apron','escape']" --set ana.path_sens[+] threadflag --set ana.base.privatization none --set ana.relation.privatization mutex-meet-tid --set ana.base.arrays.domain partitioned
=======
// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.base.privatization none --set ana.apron.privatization mutex-meet-tid --set ana.base.arrays.domain partitioned
>>>>>>> master
#include <pthread.h>
#include <assert.h>
#include <stdio.h>
#include <unistd.h>


int main(){
    int arr[] = {2, 4};

    int x,y;

    int* ptr = &y;
    x = y;

    int z = *ptr == x;
    __goblint_check(x==y);
    __goblint_check(z == 1);
    __goblint_check(*ptr == x);
    __goblint_check(*ptr == y);
    __goblint_check(y == y);

    __goblint_check(arr[*ptr == x] == 4);


    return 0;
}

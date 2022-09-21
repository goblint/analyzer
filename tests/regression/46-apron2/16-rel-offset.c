// SKIP PARAM: --set solver td3 --set ana.activated "['base','threadid','threadflag','mallocWrapper','apron','escape']" --set ana.path_sens[+] threadflag --set ana.base.privatization none --set ana.relation.privatization mutex-meet-tid --set ana.base.arrays.domain partitioned
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
    assert(x==y);
    assert(z == 1);
    assert(*ptr == x);
    assert(*ptr == y);
    assert(y == y);

    assert(arr[*ptr == x] == 4);


    return 0;
}

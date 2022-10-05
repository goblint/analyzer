//PARAM: --enable ana.int.interval
#include <stdlib.h>

int foo(int* ptr1, int* ptr2){
    int result;
    if(ptr1 == ptr2){
        result = 0;
    } else {
        result = 1;
    }
    // cram test checks for precondition invariant soundness
    return result;
}

int main(){
    int five = 5;
    int five2 = 5;
    int y = foo(&five, &five);
    int z = foo(&five, &five2);
    __goblint_check(y != z);
    return 0;
}

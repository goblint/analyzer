#include <assert.h>
int foo(int x){
    return x;
}

int main(){
    int x = 12;
    int y = foo(x);
    __goblint_check(x == y);
    return 0;
}

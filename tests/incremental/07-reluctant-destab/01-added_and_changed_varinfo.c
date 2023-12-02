#include <assert.h>
#include <math.h>

int foo(int x){
    return 1;
}

int main(){
    int x = 2;

    int a = foo(x);
    assert(a == 1);

    return 0;
}

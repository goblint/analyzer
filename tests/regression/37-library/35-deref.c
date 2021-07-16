#include<stdlib.h>

int foo(int x){
    return 0;
}

int main(){
    int *p = NULL;
    int *n = &p;

    foo(*p);
    return 0;
}

#include <stdio.h>
int main(){    
    int *pi;     // a pointer to an integer
    pi = NULL; 
    int c = foo(*pi); // WARN
    return 1;
}

int foo(int f){
    return f;
}
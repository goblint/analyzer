// PARAM: --enable ana.nullptr
#include <stdio.h>
int main(){    
    int *pi;     // a pointer to an integer
    int *t;
    int a = *t; // WARN
    t = &pi;
    pi = NULL; 
    int c = *pi; // WARN
    int d = *t; // NOWARN
    return 1;
}
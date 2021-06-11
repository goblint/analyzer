
#include <stdio.h>
int main(){    
    int *pi;     // a pointer to an integer
    int *t;
    t = 5;
    pi = NULL; 
    int c = *pi; // WARN
    int d = *t; // NOWARN
    return 1;
}
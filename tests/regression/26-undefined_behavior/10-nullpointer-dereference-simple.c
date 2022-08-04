#include <stdio.h>
int main(){    
    int *pi;     // a pointer to an integer
    int *t;
    int a = *t; // WARN
    pi = NULL; 
    int c = *pi; // WARN
    return 1;
}
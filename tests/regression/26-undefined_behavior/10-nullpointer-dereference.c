
#include <stdio.h>
int main(){    
    int *pi;     // a pointer to an integer
    int *t;
    t = 5;
    pi = NULL; 
    int c = *pi; // this is a NULL pointer dereference
    int d = *t;
    return 1;
}
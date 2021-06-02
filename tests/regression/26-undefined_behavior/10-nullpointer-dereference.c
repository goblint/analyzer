
#include <stdio.h>
struct person
{
   int age;
   float weight;
};

int main(){    
    int a, b, c; // some integers
    int *pi;     // a pointer to an integer

    pi = NULL;
    c = *pi; // this is a NULL pointer dereference
    return 1;
}
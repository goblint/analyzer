
// PARAM: --enable ana.nullptr --enable dbg.debug

#include <stdio.h>
//source base: https://stackoverflow.com/questions/4007268/what-exactly-is-meant-by-de-referencing-a-null-pointer
int main(){    
    int a, b, c, x; // some integers
    int *pi;     // a pointer to an integer
    int *ok;
    a = 5;
    ok = &a;
    pi = &a; // pi points to a
    b = *pi; // b is now 5
    pi = NULL;
    x = *ok; //Prolly nowarn
    c = *pi; // prolly warn

    return 1;
}
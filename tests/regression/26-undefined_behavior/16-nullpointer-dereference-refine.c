// PARAM: --enable sem.null-pointer.dereference-refine
#include <stdio.h>

struct person {
  int age;
  int height;
  int* ptr;
};

int main(){
    int r1, r2, r3, p;
    int *a = NULL; 
    int *b; 
    
    switch(r1) {
    case 1:
        *a = 10; // WARN
        __goblint_check(0); // NOWARN (unreachable)
        break;
    case 2:
        *a = 10; // WARN
        __goblint_check(0); // NOWARN (unreachable)
        break;
    case 3:
        *b = 10; // WARN
        p = *b;  // NOWARN
    case 4:
        if (r2) {
            struct person new_person = {1,1, NULL};
            p = *(new_person.ptr); // WARN
        } else {
            int test = 3;
            struct person non_null = {1,1, &test};
            p = *(non_null.ptr);
        }
        __goblint_check(p != NULL); // reachable
        break;
    case 5:
        if (*a) { // WARN
            p = 1;
        } else {
            p = 2;
        }
        __goblint_check(0); // NOWARN (unreachable)
        break;
    case 6:
        p = id(*a); // WARN
        __goblint_check(0); // TODO: NOWARN (unreachable)
        break;
    default:
        break;
    }
    return 1;
}

int* id(int *x) {
     return x;
}
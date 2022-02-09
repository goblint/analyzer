#include <stdio.h>

int main() {
    int a, b, x, y, z;
    if (a) {
        x = 1;
    } else {
        x = 1;
    }
    // join point label b:    

    if (b)  {
        y = 1;
    } else {
        y = 2;
    }
    // join point label a: 
    // The new domain must store
    // x-> ([1], {}),  y -> ([1,2], {(y,a)}) 
    
    z = y+1;

    // the join points for z depend on y
    // x -> ([1], y->..., z -> ([2,3],{(y,a)})
    
    if (y ==1) {
        x = 2;
    } else {
        x = 8;
    }
    // join point label c:
    // x-> ([1, 8], {c}),  y -> ([1,2], {a}) 
    assert (x > y);
    return 0;

}
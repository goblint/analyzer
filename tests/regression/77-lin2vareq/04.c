//SKIP PARAM: --set ana.activated[+] lin2vareq
// from https://dl.acm.org/doi/10.1145/2049706.2049710

#include <stdio.h>

typedef int dataX_t;
typedef int dataY_t; 

dataX_t x_arr[100];
dataY_t y_arr[100];
dataX_t *x_ptr;
dataY_t *y_ptr;

void access() {
    *x_ptr = 42;
    *y_ptr = *x_ptr + 10;
}

int main() {
    int i;
    x_ptr = &x_arr[0];
    y_ptr = &y_arr[0];

    for (i = 0; i < 100; i++) {
        access();
        
        __goblint_check(i == 8 * i + 0); //UNKNOWN!
        __goblint_check(x_ptr == 8 * i + x_arr); //UNKNOWN!
        __goblint_check(y_ptr == 4 * i + y_arr); //UNKNOWN!

        x_ptr++;
        y_ptr++;
    }

    return 0;
}

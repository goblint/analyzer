//SKIP PARAM: --set ana.activated[+] lin2vareq
// from https://dl.acm.org/doi/10.1145/2049706.2049710

#include <stdio.h>

int x;
int y;
int *x_ptr;
int *y_ptr;

void access() {
    *x_ptr = 42;
    *y_ptr = *x_ptr + 10;
}

int main() {
    int i;
    x_ptr = &x;
    y_ptr = &y;
    access();
    __goblint_check(*x_ptr == *y_ptr - 10 );
    __goblint_check(i + *x_ptr + 10 == i + *y_ptr ); //UNKNOWN!

}

// PARAM: --enable ana.int.interval --set ana.base.arrays.domain partitioned --set ana.activated[+] var_eq
#include <goblint.h>

int global;

int main(void)
{
    example1();
    return 0;
}

// Simple example
void example1(void)
{
    int top;
    int top2;
    int arr[10];

    arr[top] = 42;
    top2 = top;
    __goblint_check(arr[top2] == 42);
}

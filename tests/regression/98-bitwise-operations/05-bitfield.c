// PARAM: --enable ana.int.interval

#include <stdio.h>
#include <goblint.h>
 
struct S
{
    // three-bit unsigned field,
    // allowed values are 0...7
    int a: 4;
    unsigned int b : 3;
    int c;
    unsigned int d;
    
};
 
int main(void)
{
    int x;
    int y;
    struct S s;

    if (x) {
        y = 4;
    }
    else {
        y = 9;
    }

    s.b = -1 ;// 2147483647 + 1;
    printf("%d\n", s.b); // output: 0

    __goblint_check(s.b <= 7);
}
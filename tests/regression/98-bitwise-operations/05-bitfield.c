// PARAM: --enable ana.int.interval

#include <stdio.h>
#include <goblint.h>
 
struct S
{
    // three-bit unsigned field,
    // allowed values are 0...7
    int a: 3;
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
        y = 9; // WARN
    }

    s.a = 1;
    s.b = 1;

    __goblint_check(s.a <= 7);
    __goblint_check(s.a >= -4);

    __goblint_check(s.b <= 7);
    __goblint_check(s.a >= 0);

    s.a = 8; // WARN
    s.a = -5; // WARN
    s.b = 8; // WARN
    
    return 0; 
}
// PARAM: --enable ana.int.interval

#include <stdio.h>
#include <goblint.h>
 
struct S
{
    int a: 3;
    unsigned int b : 3;  
};
 
int main(void)
{
    int x;
    struct S s;

    if (x < -30 || x > 30) {
        x = 0;
    }
    __goblint_check(s.a <= 7);
    __goblint_check(s.a >= -4);
    __goblint_check(s.b <= 7);
    __goblint_check(s.b >= 0);
    
    s.a = 1; // NOWARN
    s.b = 1; // NOWARN

    s.a = 8; // WARN
    s.a = -5; // WARN
    s.b = 8; // WARN
    s.a = x; // WARN

    return 0; 
}
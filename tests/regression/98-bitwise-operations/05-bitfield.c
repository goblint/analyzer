// PARAM: --enable ana.int.interval

#include <stdio.h>
#include <goblint.h>
 
struct S
{
    // three-bit unsigned field,
    // allowed values are 0...7
    unsigned int b : 3;
};
 
int main(void)
{
    struct S s;
    printf("%d\n", s.b); // output: 0

    __goblint_check(s.b <= 7);
}
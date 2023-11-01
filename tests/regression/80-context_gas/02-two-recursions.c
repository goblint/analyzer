// PARAM: --enable ana.int.interval_set --enable ana.int.def_exc --enable ana.int.enums
#include<stdio.h>
#include <goblint.h>

int main(void)
{
    int res1 = f(10);
    int res2 = g(10);
    int result = res1 + res2;
    __goblint_check (result == 4); // context sensitive
    __goblint_check (result <= 6); // context insensitive
}

// gets either even or odd i values
int f(int i)
{
    if (i == 0){
        return 1;
    }
    if (i > 0){
        i--;
        g(i);
    }
    return 2;
}

// gets either even or odd i values
int g(int i)
{
    if (i == 0){
        return 3;
    }
    if (i > 0){
        i--;
        f(i);
    }
    return 4;
}



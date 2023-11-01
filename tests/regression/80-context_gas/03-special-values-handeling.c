// PARAM: --enable ana.int.interval_set --enable ana.int.def_exc --enable ana.int.enums
#include<stdio.h>
#include <goblint.h>

int main(void)
{
    int res = f(10);
    __goblint_check (res==100);
}

// gets either even or odd i values
int f(int i)
{
    if (i == 0){
        return 100;
    }
    if (i > 0){
        i--;
        f(i);
    }
    return 0;
}

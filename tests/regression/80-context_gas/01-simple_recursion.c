// PARAM: --enable ana.int.interval_set --enable ana.int.def_exc --enable ana.int.enums
#include<stdio.h>
#include <goblint.h>

int a = 5;
int main(void)
{
    f(5);
    
    __goblint_check (a==0); // FAIL
}

int f(int i)
{
    if (i > 0){
        i--;
        a = i;
        f(i);
    }
}



// PARAM: --enable ana.context.callstring --enable ana.int.interval_set
// Basic example
#include <stdio.h>

int num_iterat = 12;
int a = 20;

int f(int i)
{
    if (i > 0)
    {
        a = --i;
        f(i);
    }
    return 0;
}

int main(void)
{
    f(num_iterat);
}

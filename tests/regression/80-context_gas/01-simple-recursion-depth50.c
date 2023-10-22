// PARAM: --enable ana.int.interval
#include <goblint.h>

int global;

int main(void)
{
    f(10);
}

int f(int i)
{
    if (i > 0){
        i--;
        f(i);
    }
}

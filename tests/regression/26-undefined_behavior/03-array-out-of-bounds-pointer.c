// PARAM:  --set dbg.debug true --enable ana.arrayoob --enable ana.int.interval
//Pointer to arrays: out of bounds access
#include <stdio.h>
int main( )
{
    int arr[] = {1, 2, 3, 4, 5, 6};
    int * ptr = arr;
    ptr[3] = 3; //NOWARN
    ptr[6] = 10; //WARN
    ptr[-1] = 10; //WARN

    for (int i = 0; i < 10; ++i)
    {
        ptr[i] = 5; //WARN
    }
    return 0;
}


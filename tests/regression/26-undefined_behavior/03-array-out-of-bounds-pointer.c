// PARAM: --set ana.activated "['base','threadid','threadflag','escape','mallocWrapper']" --set dbg.debug true --enable ana.arrayoob
//Pointer to arrays: out of bounds access
#include <stdio.h>
int main( )
{
    int arr[] = {1, 2, 3, 4, 5, 6};
    int * ptr = arr;

    ptr[6] = 10; //WARN
    ptr[-1] = 10; //WARN
    for (int i = 0; i < 10; ++i)
    {
        ptr[i] = 5; //WARN
    }
    return 0;
}


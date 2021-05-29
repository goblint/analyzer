// PARAM: --set ana.activated "['base','threadid','threadflag','escape','mallocWrapper']" --set dbg.debug true --enable ana.arrayoob
//Pointer pointing to arrays of different sizes
#include <stdio.h>
int main( )
{
    int arr[] = {1, 2, 3, 4, 5, 6};
    int arr2[] = {1, 2, 3};
    int * ptr = arr;

    ptr[6] = 10; //WARN
    ptr[-1] = 10; //WARN
    for (int i = 0; i < 10; ++i)
    {
        ptr[i] = 5; //WARN
    }
    int * ptr = arr2;
    ptr[3] = 10; //WARN
    ptr[-1] = 10; //WARN
    for (int i = 0; i < 5; ++i)
    {
        ptr[i] = 5; //WARN
    }
    return 0;
}


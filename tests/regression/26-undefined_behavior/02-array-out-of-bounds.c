// PARAM: --set ana.activated "['base','threadid','threadflag','escape','mallocWrapper']" --set dbg.debug true --enable ana.arrayoob
#include <stdio.h>

int main()
{
    int arr[] = {1, 2, 3, 4, 5, 6};
    arr[2] = 0; //NOWARN
    arr[6] = 10; //WARN
    arr[-1] = 10; //WARN
    for (int i = 0; i < 5; ++i)

    for (int i = 0; i < 10; ++i)
    {
        arr[i] = 5; //WARN
    }
    return 0;
}

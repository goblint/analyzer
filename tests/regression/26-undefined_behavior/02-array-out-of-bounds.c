// PARAM:  --set dbg.debug true --enable ana.arrayoob --enable ana.int.interval --enable ana.int.enums
#include <stdio.h>
//This is the most basic case
int main()
{
    int arr[] = {1, 2, 3, 4, 5, 6};
    arr[2] = 0; //NOWARN
    arr[6] = 10; //WARN
    arr[-1] = 10; //WARN

    for (int i = 0; i < 10; ++i)
    {
        arr[i] = 5; //WARN
    }
    return 0;
}

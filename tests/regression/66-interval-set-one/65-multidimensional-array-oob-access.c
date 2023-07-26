// PARAM: --enable ana.arrayoob --enable ana.int.interval_set --enable ana.int.enums
//Multidimensional array: Out of bounds access
#include <stdio.h>
int main( )
{
    int arr[4][3] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12};
    arr[3][2] = 3; //NOWARN
    arr[6][3] = 10; //WARN
    arr[3][6] = 10; //WARN
    arr[0][1] = 4; //NOWARN
    arr[-6][3] = 10; //WARN
    arr[3][-3] = 10; //WARN

    for (int i = 0; i < 10; ++i)
    {
        arr[i][i] = 5; //WARN
    }
    return 0;
}


//Dynamically sized array: oob access
#include <stdio.h>
int main()
{
    arr[0] = 1;
    arr[1] = 2;
    arr[2] = 3;
    arr[3] = 4;
    arr[4] = 5;
    arr[-1] = 10; //WARN
    for (int i = 0; i < 5; ++i)
    {
        arr[i] = 5; //WARN
    }
    return 0;
}

// Program to demonstrate 
// accessing array out of bounds
#include <stdio.h>
int main()
{
    int arr[2] = {1,2};      
    // arr[10] is out of bound
    //printf("arr[10] is %d\n", arr[10]);
    test(arr[10]);
    return 0;
}

int test(int b){
    return 0;
}
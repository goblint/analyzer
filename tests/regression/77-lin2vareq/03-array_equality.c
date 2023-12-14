//SKIP PARAM: --set ana.activated[+] lin2vareq
#include <stdio.h>

int main() {
    int arr1[] = {1, 2, 3};
    int arr2[] = {1, 2, 3};
    
    int x1 = arr1[0];
    int x2 = arr2[0];
    
    int y1 = arr1[1];
    int y2 = arr2[1];

    __goblint_check(x1 == x2); //SUCCESS
    __goblint_check(y1 == y2); //SUCCESS

    return 0;
}

//In this case, variables are introduced to represent the values of array elements at specific indices. The equality checks are performed on those variables.

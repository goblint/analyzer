//SKIP PARAM: --set ana.activated[+] lin2vareq
#include <stdio.h>

int main() {
    int arr1[] = {1, 2, 3};
    int arr2[] = {1, 2, 3};
    int ptr1[] = arr1;
    int ptr2[] = arr2;

    for(int i=0; i< 3 ; i++){
        __goblint_check(ptr1[i] ==ptr2[i]); //SUCCESS
    }
    
    return 0;
}

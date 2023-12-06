#include <stdio.h>

int main() {
    int arr1[] = {1, 2, 3};
    int arr2[] = {1, 2, 3};
    int ptr1 = arr1;
    int ptr2 = arr2;

    __goblint_check(ptr1 ==ptr2); //SUCCESS

    return 0;
}

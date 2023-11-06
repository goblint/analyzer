#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {

    srand(time(NULL));
    unsigned int length =  100 ;
    int arr[100];
    for (int i = 0; i < 100; i++) {
        arr[i] = 1;
    }
    
    // for (int i = 0; i < length; i++) {
    //     printf("%d ", arr[i]);
    // }
    return 0;

}
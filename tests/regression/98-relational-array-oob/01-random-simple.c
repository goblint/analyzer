//PARAM: --enable ana.arrayoob    --set ana.activated[+] apron   
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() 
{
    srand(time(NULL));
    unsigned int len =   (rand()%32) +3;
    int arr[len];

    for (unsigned int i = 0 ; i < len  -1; i++) {
        int t = arr[i] +3;
        arr[i ] =   32;
    }
    
    for (unsigned int i = 0; i < len+1; i++) {
        printf("%d ", arr[i]); //WARN
    }
    return 0;
}

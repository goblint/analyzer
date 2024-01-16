//PARAM: --enable ana.arrayoob    --set ana.activated[+] apron   --set ana.apron.domain polyhedra   

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() 
{
    srand(time(NULL));
    unsigned int len =   (rand()%32) +3;
    int arr[len];

    for (int i = 0 ; i < len  ; i++) {
        int t = arr[i] +3; //NOWARN
        arr[i] =   32; //NOWARN
    }
    
    return 0;
}

//PARAM: --enable ana.arrayoob  --enable ana.int.interval --set ana.activated[+] apron  --set ana.apron.domain octagon  

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() 
{
    srand(time(NULL));
    int len =   (rand()%32) +2;
    int len2 =   (rand()%32) +2;
    int len3 =   (rand()%32) +2;
    int arr[len][len2][len3];

    for (  int i = 0 ; i < len  ; i++) {
        for (  int j = 0; j < len2; j++){
            for (int k = 0; k < len3; k++){
                arr[i][j][k] = 3; //NOWARN
                int f = arr[i][j][k]; //NOWARN
            }
        }
    }
    
    
    return 0;
}

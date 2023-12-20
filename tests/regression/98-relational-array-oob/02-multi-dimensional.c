//PARAM: --enable ana.arrayoob  --enable ana.int.interval --set ana.activated[+] apron  --set ana.apron.domain polyhedra   
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() 
{
    srand(time(NULL));
    unsigned  int len =   (rand()%32) +2;
    unsigned int len2 =   (rand()%32) +1;
    unsigned int len3 =   (rand()%32) +1;
    unsigned arr[len ][len2][len3];
    for ( unsigned int i = 0 ;i < len ; i++) {
        for ( unsigned  j = 0 ; j < len2; j++)
            for(unsigned int k = 0; k < len3; k++){
                    arr[i][j][k] = 0;
                    arr[i][j][k] = 3;
                    int t = arr[i][j][k];
    }
    }
    return 0;
}

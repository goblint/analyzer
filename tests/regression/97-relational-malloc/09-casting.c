// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --set ana.activated[+] apron  --set ana.apron.domain polyhedra --set sem.int.signed_overflow assume_none
#include <stdio.h>
#include <stdlib.h>
#include <time.h>


int main()
{

    unsigned int len = rand();

    int* p = malloc(sizeof(int) * len);

    char * t = p; 
    for (int i = 0; i < sizeof(int) * len /sizeof(char); i++)
    {
        *(t + i) = 2;
        *(t+1) = 2;
        char tmp = *(t + i);
    }
    
    free(p);

    long *p = malloc(sizeof(long) * len);

    int * t = p; 
    for (int i = 0; i < sizeof(long)/sizeof(int) * len ; i++)
    {
        *(t + i) = 2;
        *(t+1) = 2;
        int tmp = *(t + i);
    }
    
    free(p);
}

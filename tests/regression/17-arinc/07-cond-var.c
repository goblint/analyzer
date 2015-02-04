#include <stdlib.h>
#include <time.h>
#include <stdio.h>

int main(){
    /* time_t t; */
    /* srand((unsigned) time(&t)); */
    /* int i = rand() % 2000; */
    int i;
    int b;
    printf("i = %d ", i);
    b = i < 1000;
    if(b){
        printf("<");
    }else{
        printf(">=");
    }
    printf(" 1000\n");
    return 0;
}

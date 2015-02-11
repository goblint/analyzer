#include <stdlib.h>
#include <time.h>
#include <stdio.h>

int main(){
    /* time_t t; */
    /* srand((unsigned) time(&t)); */
    /* int i = rand() % 2000; */
    int i;
    int a, b;
    printf("i = %d ", i);
    a = i < 1000;
    if(a){
        printf("<");
    }else{
        printf(">=");
    }
    b = a;
    if(b){
        printf("<");
    }else{
        printf(">=");
    }
    printf(" 1000\n");
    return 0;
}

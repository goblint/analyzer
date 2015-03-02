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
    a = (int) ((int) i < 1000); // casts shouldn't be a problem
    if(a){
        printf("<");
    }else{
        printf(">=");
    }
    /* a = 1; // this should destroy b -> i < 1000 */
    b = a;
    a = 1; // this shouldn't destroy b -> i < 1000
    if(b){
        printf("<");
    }else{
        printf(">=");
    }
    printf(" 1000\n");
    return 0;
}

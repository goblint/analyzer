#include <stdlib.h>
#include <stdio.h>

// expressions for a should not escape their scope
int a;

void f(){
    int i;
    if(a){ // Unknown
        printf("<");
    }else{
        printf(">=");
    }
    a = i < 2; // this should not move to main
}

int main(){
    int i;
    a = i < 1; // this should not move to f
    f();
    if(a){ // Unknown
        printf("<");
    }else{
        printf(">=");
    }
    {
        int i;
        a = i < 3; // this should not escape
    }
    if(a){ // Unknown
        printf("<");
    }else{
        printf(">=");
    }
    return 0;
}

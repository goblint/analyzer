// PARAM: --set ana.activated "['base']"  --set ana.int.interval true --set ana.int.trier false --set ana.int.relational true --set ana.int.equations true
#include<stdio.h>
#include<assert.h>


int main () {
    int a = 1,b = 2,c = 3;
    int z;
    
    if (z != 0){
        a = 8;
        b = 9;
    } else {
        a = 9;
        b = 8;
    }
    c = a + b;
    
    return 0;
}


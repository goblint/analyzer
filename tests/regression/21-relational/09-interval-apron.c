// PARAM: --set ana.activated "['base']"  --set ana.int.interval true --set ana.int.trier false --set ana.int.relational true --set ana.int.aprondomain true
#include<stdio.h>
#include<assert.h>


int main () {
    int a = 1,b = 2,c = 3;
    int x,y,z;
    int false = 0;
    int true = 42;
    
    if (x){
        assert(x != 0);//NOWARN

    } else {
        assert(x == 0);
    }
    
    assert(!! true);
    assert(!  false);
    
    if (a){
        a = a;
    } else
        assert(0); //NOWARN
    
    
    if (!a)
        assert(0); //NOWARN
    else
        a = a;
    
    if (z != 0){
        a = 8;
        b = 9;
    } else {
        a = 9;
        b = 8;
    }
    
    assert(a);
    assert(a!=b); //NOWARN
    assert(a<10);
    assert(a<=9);
    assert(!(a<8));
    assert(a==8);//NOWARN
    assert(b>7);
    assert(b>=8);
    assert(!(a>9));
    assert(b==8); //FAIL
    
    for(x = 0; x < 10; x++){
        assert(x >= 0);
        assert(x <= 9);
    }
    assert(x == 10); //NOWARN
    
    return 0;
}


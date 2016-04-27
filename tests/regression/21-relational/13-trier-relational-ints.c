// PARAM: --set ana.activated "['base']" --set ana.int.trier true --set ana.int.relational true --set ana.int.equations true
#include<stdio.h>
#include<assert.h>


int main () {
    int a = 1,b = 2,c = 3;
    int x,y,z;
    int false = 0;
    int true = 42;
    
    if (x){
        assert(x != 0);

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
    a = 8;
    b = 9;
    assert(a!=b);
    a = 9;
    
    assert(a!=8);
    assert(a == 9);
    assert(a == b);
    assert(a != b); //FAIL
    return 0;
}


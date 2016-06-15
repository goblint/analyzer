// PARAM: --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier false --set ana.structs.relational true --set ana.structs.equations true --set ana.equation.plus false

typedef struct  {
    int i;
    int k;
} S;

int main(){
    int a;
    S p;
    if (a > 0) {
        p.i = 5;
        p.k = 6;
    } else {
        p.i = 6;
        p.k = 5;
    }
    
    assert (p.i != p.k);
    
    if (p.i == p.k) {
        p.i = 5;
    } else {
        p.i = 6;
    }
    
    assert (p.i == 6);
    assert (p.i == p.k); //FAIL
}
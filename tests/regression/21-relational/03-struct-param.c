// PARAM: --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier true --set ana.int.queries true --set ana.int.relational true --set ana.int.equations true --set ana.int.queries true --set ana.structs.relational true --set ana.structs.relational_to_analyze "['S']"
typedef struct  {
    int i;
    int k;
    
} S;

S some_function(S test){
    S xx;
    xx = test;
    assert(xx.i == 5); //NOWARN
    assert(xx.k == 7); //NOWARN
    int z;
    z = 5;
    for (z = 5; z > 0; z--) {
        xx.i = xx.i - 1;
    }
    return xx;
}

int main(){
    S a;
    a.i = 5;
    a.k = 7;
    a = some_function(a);
    return a.i;
}
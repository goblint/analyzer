// PARAM: --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier true --set ana.int.queries true --set ana.int.relational true --set ana.int.equations true --set ana.int.queries true --set ana.structs.relational true --set ana.structs.relational_to_analyze "['S']"
typedef struct  {
    int i;
    int k;
} S;

S some_function(S test){
    S xx;
    xx = test;
    assert(xx.i == 4);
    xx.i = 5;
    assert(xx.i == 5);
    assert(xx.k == 7); 
    S yy;
    yy.i = 7;
    int z;
    z = 5;
    return yy;
}

int main(){
    S a;
    a.i = 4;
    a.k = 7;
    S b;
    b = some_function(a);
    assert(b.i == 7);
    assert(a.i == 4);
    return a.i;
}
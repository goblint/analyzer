// PARAM: --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier true --set ana.int.queries true --set ana.int.relational true --set ana.int.equations true --set ana.int.queries true

int some_function(int test){
    int xx;
    xx = test;
    assert(xx == 4);
    xx = 5;
    assert(xx == 5);
    int yy;
    yy = 7;
    int z;
    z = 5;
    return yy;
}

int main(){
    int a;
    a = 4;
    int b;
    b = some_function(a);
    assert(a == 4);
    assert(b == 7);
    return a;
}
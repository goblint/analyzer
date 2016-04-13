// PARAM: --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier true --sets solver slr3 --set ana.int.relational true --set ana.int.equations true --set ana.int.queries true


int some_function(){
    int xx;
    xx = 5;
    int z;
    z = 5;
    for (z = 5; z > 0; z--) {
        xx = xx - 1;
    }
    assert (z == 0);
    assert (z < 1);
    return xx;
}

int main(){
    int a;
    a = some_function();
    return a;
}
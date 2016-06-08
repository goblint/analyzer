// PARAM: --set ana.activated "['base']" --set ana.int.interval true --set ana.int.trier false --set ana.int.relational true --set ana.int.equations true --set ana.equation.plus false

int main(){
    int a, b, c;
    if (a > 0) {
        b = 5;
        c = 6;
    } else {
        b = 6;
        c = 5;
    }
    
    assert (c != b);
    
    if (c == b) {
        c = 5;
    } else {
        c = 6;
    }
    
    assert (c == 6);
    assert (b == c); //FAIL
}
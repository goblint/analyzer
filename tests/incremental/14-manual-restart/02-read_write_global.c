int g = 0;

void foo(){
    g = 1;
}

void bar(){
    int x = g;
    __goblint_check(x % 2 == 0); // UNKNOWN (imprecision caused by earlyglobs)
}

int main(){
    foo();
    bar();
    return 0;
}

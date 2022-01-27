int g = 0;

void foo(){
    g = 1;
}

void bar(){
    int x = g;
    assert(x % 2 == 0); // UNKNOWN
}

int main(){
    foo();
    bar();
    return 0;
}

//PARAM: --sets ana.activated[+] constants
int f(int a, int b){
    int d = 3;
    return a + d;
}

int main(){
    int x = 3;
    int y = x;
    int z = y;
    int *b = &z;

    *b = 2;

    int a;
    int b;
    while(x < 10){
        a = 13;
        b = x;
        x++;
    }

    int c = f(2, 3);
    return 0;
}

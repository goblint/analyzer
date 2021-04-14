//PARAM: --sets ana.activated[+] constants
int f(int a, int b){
    int d = 3;
    return a + d;
}

int main(){
    int a;
    int b;
    int x = 3;
    int y = x;
    int z = y;
    int w = x + y;
    int *p = &z;

    *p = 2;

    while(x < 10){
        a = 13;
        p = x;
        x++;
    }

    int c;
    if(x){
        c = 0;
    } else {
        c = 1;
    }

    int d = f(2, 3);
    return 0;
}

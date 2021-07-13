//PARAM: --set ana.activated '["constants"]'
int f(int a, int b){
    int d = 3;
    int z = a + d;
    return z;
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
    if(y){
        c = 0;
    } else {
        c = 1;
    }

    int d = 0;
    d = f(2, 3);
    return 0;
}

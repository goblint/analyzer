//PARAM: --set ana.activated '["constants", "ptranal"]'
// intentional explicit ana.activated to do tutorial in isolation
int f(int a, int b){
    int d = 3;
    int z = a + d;
    return z;
}

int main(){
    int d = 0;
    int (*fp)(int,int) = &f;
    d = fp(2, 3);
    return 0;
}

int main () {
    f(50,50);
}

int f (int x, int y){
    if (x == 0){
        __goblint_check (y == 0);
        return 0;
    }
    f (--x, --y);
}
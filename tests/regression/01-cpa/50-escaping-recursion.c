int rec(int i,int* ptr) {
    int top;
    int x = 17;

    if(i == 0) {
        rec(5,&x);
        // Recursive call may have modified x
        assert(x == 17); //UNKNOWN!
    } else {
        x = 31;

        // ptr points to the outer x, it is unaffected by this assignment
        // and should be 17
        assert(*ptr == 31); //UNKNOWN!

        if(top) {
            ptr = &x;
        }

        // ptr may now point to both the inner and the outer x
        *ptr = 12;
        assert(*ptr == 12); //UNKNOWN!
        assert(x == 12); //UNKNOWN!
    }
    return 0;
}


int main() {
    int t;
    rec(0,&t);
    return 0;
}

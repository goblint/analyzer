// PARAM: --sets sem.int.signed_overflow assume_none
int main(void) {
    int a;
    int top;
    int s;

    if(a != -1) {
        int s = a+1;
        assert(s != 0);
    }
}

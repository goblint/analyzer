//PARAM: --disable ana.int.def_exc --enable ana.int.interval
int main() {
    int x;

    if(!x) {
        assert(x==0);
    } else {
        assert(x==1); //UNKNOWN!
    }
}

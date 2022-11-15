// PARAM: --set trans.activated[+] "assert"

// Running the assert transformation on this test used to yield code that cannot be compiled with gcc, due to superfluous offsets on a pointer
struct s {
    int a;
    int b;
};

union u {
    struct s str;
    int i;
};


int main(){
    union u un;

    struct s* ptr;

    un.str.a = 1;
    un.str.b = 2;

    ptr = &un.str;
    int r;
    int x;
    if(r){
        x = 2;
    } else {
        x = 3;
    }

    return 0;
}

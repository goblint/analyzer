//PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>
#include <alloca.h>

int *f() {
    int *c = alloca(sizeof(int));
    return c;
}

int main(int argc, char const *argv[]) {
    int *ps = alloca(sizeof(int));
    int *c = f();
    int a = *ps;
    int b = *c; //WARN
    return 0;
}

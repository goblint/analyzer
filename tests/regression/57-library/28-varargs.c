// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs

#include <stdarg.h>

int sum(int count, ...){
    int sum;
    va_list arg;
    va_start(arg, count);

    for(int i=0; i<count; i++){
        sum = va_arg(arg, int);
    }

    return sum;
}

void write_all(int count, ...){
    va_list arg;
    va_start(arg, count);

    int top;
    if(top){
        for(int i=0; i<count; i++){
            int* ptr = va_arg(arg, int*);
            *ptr = 2;
        }
    }
    return;
}

int main(){
    int x = 1;
    int y = 2;
    int z = 3;
    int count = 3;

    write_all(count, &x, &y, &z);
    assert(x == 1); // UNKNOWN!
    return 0;
}

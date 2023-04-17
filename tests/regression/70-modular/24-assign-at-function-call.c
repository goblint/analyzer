//SKIP PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'"
#include<goblint.h>

int five(){
    return 5;
}

void write_arg(int *x){
    *x = five();
}

void call_delegate_writing(){
    int x = 3;
    __goblint_check(x == 3);
    write_arg(&x);
    __goblint_check(x != 3); //UNKNOWN
    __goblint_check(x == 5); //UNKNOWN
}

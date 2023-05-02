//PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'"
#include<goblint.h>

void write_arg(int *x){
    *x = 10;
}

void delegate_writing(int *x){
    write_arg(x);
}

void call_delegate_writing(){
    int x = 3;
    __goblint_check(x == 3);
    delegate_writing(&x);
    __goblint_check(x != 3); //UNKNOWN
}

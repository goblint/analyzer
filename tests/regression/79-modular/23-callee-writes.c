//PARAM: --enable modular --set ana.modular.funs "['write_arg', 'delegate_writing']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'"  --set ana.activated[+] "'used_globals'"
#include<goblint.h>

void write_arg(int *x){
    *x = 10;
}

void delegate_writing(int *x){
    write_arg(x);
}

int main(){
    int x = 3;
    __goblint_check(x == 3);
    delegate_writing(&x);
    __goblint_check(x != 3); //UNKNOWN
    return 0;
}

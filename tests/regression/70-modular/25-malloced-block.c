//SKIP PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'" --enable ana.int.interval
#include<goblint.h>
#include<stdlib.h>

void modify(int *i){
    if(i != NULL)
        *i = 12;
}

void allocate_and_call_modify(){
    int j;
    int* m = malloc(sizeof(int));
    *m = 0;
    __goblint_check(*m == 0);
    modify(m);
    __goblint_check(*m != 0); //UNKNOWN
    __goblint_check(*m == 12); //UNKNOWN
    __goblint_check(0 <= *m);
    __goblint_check(*m <= 12);

    j = *m;

    __goblint_check(j != 0); //UNKNOWN
    __goblint_check(j == 12); //UNKNOWN
    __goblint_check(0 <= j);
    __goblint_check(j <= 12);


}

int main(){
    allocate_and_call_modify();
}
//PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'"
#include<goblint.h>

#define X_VALUE 12
#define Y_VALUE 3

typedef struct {
    int x;
    int y;
} tuple_t;

int modify_x(tuple_t *p){
    p->x = 123;
}

int modify_y(tuple_t *p){
    p->y = 13;
}

int foo(){
    tuple_t t = {X_VALUE, Y_VALUE};

    __goblint_check(t.x == X_VALUE);
    __goblint_check(t.y == Y_VALUE);

    modify_x(&t);

    __goblint_check(t.x != X_VALUE); //UNKNOWN
    __goblint_check(t.y == Y_VALUE);

    modify_y(&t);

    __goblint_check(t.x != X_VALUE); //UNKNOWN
    __goblint_check(t.y != Y_VALUE); //UNKNOWN
}

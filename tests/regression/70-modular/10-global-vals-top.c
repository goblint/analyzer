//SKIP PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'taintPartialContexts'"

typedef struct {
    int x;
    int y;
} tuple_t;

tuple_t g = {0, 0};

int write_global(){
    __goblint_check(g.x == 0); // UNKNOWN! (May be modified by other code)
    g.x = 0;
    __goblint_check(g.x == 0); // UNKNOWN! (May be modified by other code in parallel)
}

int write_param(tuple_t tuple){
    __goblint_check(tuple.x == 0); // UNKNOWN! (May be modified by other code)
    tuple.x = 0;
    __goblint_check(tuple.x == 0);
}

int write_param_pointer(tuple_t *tuple){
    __goblint_check(tuple->x == 0); // UNKNOWN! (May be modified by other code)
    tuple->x = 0;
    __goblint_check(tuple->x == 0); // UNKNOWN! (May be modified by other code in parallel)
}

// SKIP PARAM: --set ana.activated[+] apron --set ana.base.privatization none --set ana.relation.privatization top
#include <goblint.h>

int g = 0;
int h = 0;
int *ptr = &g;

int change_ptr_and_return_5() {
    ptr = &h;
    return 5;
}

int main() {
    // ptr points to h here, but apron evaluates on pre-state of function call, updating g instead
    *ptr = change_ptr_and_return_5();

    int x = g + 1; // Currently this fails because of operation on bot
    assert(g == h); //FAIL
    assert(h == 5);
    assert(g == 0);
}

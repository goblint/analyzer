#include <goblint.h>

int main() {
    int x;

    if(!x) {
        __goblint_check(x==0);
    } else {
        __goblint_check(x==1); //UNKNOWN!
    }
}

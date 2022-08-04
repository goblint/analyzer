#include<limits.h>
#include <assert.h>

int main(void) {
    int x = INT_MAX + 1;
    __goblint_check(x == INT_MIN); //UNKNOWN!

    int r = - INT_MIN;

    __goblint_check(r == INT_MIN); //UNKNOWN!
}

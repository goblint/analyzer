#include<limits.h>

int main(void) {
    int x = INT_MAX + 1;
    assert(x == INT_MIN); //UNKNOWN!

    int r = - INT_MIN;

    assert(r == INT_MIN); //UNKNOWN!
}

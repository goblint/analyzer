#include <goblint.h>
int* ptr;
int nine = 9;

int other() {
    __goblint_check(*ptr == 8); //UNKNOWN!
}

int main()
{
    int g = 8;
    int top;

    if(top) {
        ptr = &g;
    } else {
        ptr = &nine;
    }

    other();
}

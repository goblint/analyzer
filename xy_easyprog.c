#include <stdlib.h>

int f(int x, int y) {
    int i = 2;
    
    if (x > 0) {
        i = i + 2;
        return i + y;
    } else {
        i = i + 3;
        return i + x;
    }
}

int g (int x, int y) {
    int i = 2;
    
    if (x > 0) {
        i = i + 2;
        return i;
    } else {
        i = i + 3;
        return i + x;
    }
}


int main() {
    int a = 0;
    int c = 3;

    int rand;

    int (*h)(int, int); // function pointer to f
    h = &f;

    // if (rand) {
    //     h = &g;
    // }
    int d = f (a, c);
    
    // a = -100;

    // int b = (*h)(a, c);
    return d;
}

//git diff --cached --name-only --diff-filter=ACM | grep -E '\.(ml|mli)$' | xargs -I {} ocp-indent -i {}
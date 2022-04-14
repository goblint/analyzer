// PARAM: --set "ana.activated[+]" taint --disable warn.imprecise --set "exp.extraspecials[+]" printInt

// getchar from the standard library is marked as a source
int getchar() __attribute__((__taint_source__));

// putchar from the standard library is marked as a sink
int putchar(int c)  __attribute__((__taint_sink__));

// we can also declare our own functions to be sources / sinks
void printInt(int x,int y ,int z) __attribute__((__taint_sink__));

int id(int x) {
    return x;
}

int benign(int x) {
    return 0;
}

int fun(int x,int y) {
    if(x == 5) {
        return x+y;
    } else {
        return x;
    }
}

int contextNotSufficient() {
    // This is an advanced example that shows that our analysis still has surprising false positives
    // To fix these, more context would be required
    // If you are adventurous, you might try to modify the analysis such that it is context-sensitive
    // and succeeds here as well

    int x;
    int y;

    int r = fun(x,y);
    printInt(r,r,r); // Here, we would hope for no warn, but we actually get a spurious warning, as we are not using enough context in the example
    x = getchar();
    y = getchar();

    r = fun(x,y);
    printInt(r,r,r); //WARN
}


int main(void) {
    int x = getchar();
    int y;

    x = id(x);
    printInt(x,x,x); //WARN

    int r = fun(8,fun(6,id(x)));
    printInt(r,r,r); //NOWARN

    r = fun(x,x);
    printInt(r,r,r); //WARN

    // Another example of the analysis benefitting from base
    r = y==5 ? fun(8,x) : fun(y,x);
    printInt(r,r,r); //NOWARN


    contextNotSufficient();
}

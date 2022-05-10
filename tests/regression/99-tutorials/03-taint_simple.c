// SKIP PARAM: --set "ana.activated[+]" taint --disable warn.imprecise --set "exp.extraspecials[+]" printInt

// getchar from the standard library is marked as a source
int getchar() __attribute__((__taint_source__));

// putchar from the standard library is marked as a sink
int putchar(int c)  __attribute__((__taint_sink__));

// we can also declare our own functions to be sources / sinks
void printInt(int x,int y ,int z) __attribute__((__taint_sink__));

int main(void) {
    int x = getchar();
    int y;
    int tmp;

    printInt(1,2,3); //NOWARN
    printInt(x,x,x); //WARN
    printInt(x+7,2,3); //WARN
    printInt(0,x-2*x,3); //WARN

    x = x+7;
    printInt(x,x,x); //WARN

    x = 8;
    printInt(x,x,x); //NOWARN

    y = x+17;
    y = y/getchar();
    printInt(y,y,y); //WARN


    // Trivial example showing how the analysis you just wrote benefits from other analyses
    // If we wanted to write a real analysis, we would also aks other analyses questions, to e.g. handle pointers
    int z;
    if(z == 0) {
        z = 5;
    }

    if(z == 0) {
        z = getchar();
    }

    printInt(z,z,z); //NOWARN
}

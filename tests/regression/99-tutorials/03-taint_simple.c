// SKIP PARAM: --set "ana.activated[+]" taint --set "exp.extraspecials[+]" readInt --set "exp.extraspecials[+]" printInt  --disable warn.imprecise

int readInt() __attribute__((__taint_source__));
void printInt(int x,int y ,int z) __attribute__((__taint_sink__));


int main(void) {
    int x = readInt();
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
    y = y/readInt();
    printInt(y,y,y); //WARN


    // Trivial example showing how the analysis you just wrote benefits from other analyses
    // If we wanted to write a real analysis, we would also aks other analyses questions, to e.g. handle pointers
    int z;
    if(z == 0) {
        z = 5;
    }

    if(z == 0) {
        z = readInt();
    }

    printInt(z,z,z); //NOWARN
}

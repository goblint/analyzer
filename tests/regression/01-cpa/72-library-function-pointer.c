#include <fcntl.h>
#include <unistd.h>

typedef void (*fnct_ptr)(void);
typedef int (*open_t)(const char*,int,...);
typedef int (*ftruncate_t)(int,off_t);

// Goblint used to crash on this example because the number of supplied arguments for myopen is bigger than
// than the expected number of arguments for the library function descriptior of ftruncate.
//  -- "open" expects 3 arguments, while "ftruncate" expects only 2.
int main(){
    fnct_ptr ptr;
    int top, top2, top3;

    if (top){
        ptr = (fnct_ptr) &open;
        ftruncate_t myftruncate;
        myftruncate = (ftruncate_t) ptr;
        myftruncate(0, 100); //NOWARN
    } else {
        ptr = (fnct_ptr) &ftruncate;
    }

    if (top2) {
        open_t myopen;
        myopen = (open_t) ptr;
        // Warn about possible call to ftruncate with wrong number of arguments
        myopen("some/path", 0, 0); // WARN
    } else if(top3) {
        ftruncate_t myftruncate2;
        myftruncate2 = (ftruncate_t) ptr;
        off_t v = 100;
        // We (currently) only warn about wrong number of args, not wrong type
        // So no warning is emitted here about possibly calling the vararg function open.
        myftruncate2(0, v);
    } else {
        // Warn about potential calls to open and ftruncate with too few arguments, 
        // and warn that none of the possible targets of the pointer fit as call targets.
        ptr(); // WARN
    }
    return 0;
}

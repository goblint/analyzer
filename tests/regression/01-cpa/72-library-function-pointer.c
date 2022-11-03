#include <fcntl.h>
#include <unistd.h>

typedef void (*fnct_ptr)(void);
typedef int (*open_t)(const char*,int,int);
typedef int (*ftruncate_t)(int,off_t);

// Goblint used to crash on this example because the number of supplied arguments for myopen is bigger than
// than the expected number of arguments for the library function descriptior of ftruncate.
//  -- "open" expects 3 arguments, while "ftruncate" expects only 2.
int main(){
    fnct_ptr ptr;
    int top;

    if (top){
        ptr = &open;
    } else {
        ptr = &ftruncate;
    }

    if (top) {
        // Some (nonsensical, but compiling) call to open
        open_t myopen;
        myopen = (open_t) ptr;
        myopen("some/path", O_CREAT, 0);
    } else {
        // Some (nonsensical, but compiling) call to ftruncate
        ftruncate_t myftruncate;
        myftruncate = (ftruncate_t) ptr;
        myftruncate(0, 100);
    }
    return 0;
}

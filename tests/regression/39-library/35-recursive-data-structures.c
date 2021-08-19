// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs
#include<stdlib.h>
struct _IO_FILE;

typedef struct _IO_FILE FILE;

struct _IO_FILE {
   unsigned char *rend ;
   int (*close)(FILE * ) ;
   unsigned char *wend ;

   FILE *prev ;
   FILE *next ;
   int fd ;

   FILE *prev_locked ;
   FILE *next_locked ;
};


int foo(FILE *test){

    int v = test->next->fd;


    return v;
}

int main(){
    FILE* file = malloc(sizeof(FILE));

    foo(file);

    return 0;
}

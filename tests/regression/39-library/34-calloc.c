// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs
#include <stdlib.h>

// extract from milc_comb.c that crashed the analyzer

typedef int int32type;
typedef unsigned int u_int32type;
typedef int field_offset;

struct __anonstruct_gauge_header_30 {
   int32type magic_number ;
   char time_stamp[64] ;
   int32type dims[4] ;
   int32type header_bytes ;
   int32type order ;
};
typedef struct __anonstruct_gauge_header_30 gauge_header;
struct __anonstruct_gauge_check_31 {
   u_int32type sum31 ;
   u_int32type sum29 ;
};
typedef struct __anonstruct_gauge_check_31 gauge_check;
struct __anonstruct_gauge_file_35 {
   gauge_header *header ;
   char *filename ;
   int byterevflag ;
   int32type *rank2rcv ;
   int parallel ;
   gauge_check check ;
};
typedef struct __anonstruct_gauge_file_35 gauge_file;


int foo(int length, int* ptr){
    int top;

    gauge_file* gf = calloc(1, sizeof(gauge_file));
    gauge_header* gh = calloc(1, sizeof(gauge_header));


    gf->header = gh;

    void *p = ptr;
    *(int* )p = 23;
    return gf;
}

gauge_file *bar(){
    int *a;
    int *pp = &a;
    gauge_file *p;
    for(int i = 0; i<100; i++){
        p = foo(100, pp);
    }
    return p;
}

int main(){
    gauge_file* gf = bar();
    return 0;
}

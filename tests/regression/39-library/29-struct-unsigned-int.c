// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs


struct bar {
   unsigned int x;
   unsigned int y;
};


int foo(struct bar* b){
    if(!b->x){ // Used to crash here, because of wrong ikind that was used for fields in b
        return 0;
    }

    return 1;
}

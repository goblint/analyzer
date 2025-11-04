// PARAM: --enable ana.int.interval --set ana.activated[+] taintPartialContexts
// Adapted from sv-benchmarks/termination-crafted-lit/cstrcmp.c
typedef long unsigned int size_t;

void * __attribute__((__cdecl__)) malloc (size_t __size) ;

extern int __VERIFIER_nondet_int(void);

/* Returns some null-terminated string. */
char* build_nondet_String(void) {
    int length = __VERIFIER_nondet_int();
    if (length < 1) {
        length = 1;
    }
    char* nondetString = (char*) malloc(length * sizeof(char));
    nondetString[length-1] = '\0';
    return nondetString;
}





int (cstrcmp)(const char *s1, const char *s2)
 {
     unsigned char uc1, uc2;
     /* Move s1 and s2 to the first differing characters 
        in each string, or the ends of the strings if they
        are identical.  */
     while (*s1 != '\0' && *s1 == *s2) {
         s1++;  //NOWARN!
         s2++;  //NOWARN!
     }
     /* Compare the characters as unsigned char and
        return the difference.  */
     uc1 = (*(unsigned char *) s1);
     uc2 = (*(unsigned char *) s2);
     return ((uc1 < uc2) ? -1 : (uc1 > uc2));
 }

int main() {
    return cstrcmp(build_nondet_String(),build_nondet_String());
}



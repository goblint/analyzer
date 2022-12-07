//PARAM: --disable ana.base.limit-string-addresses
#include <stdlib.h>
#include <goblint.h>

int rand();
char *unknown_function();

int main(){
    char* str = "Hello";
    char* str2 = "Hello";
    char* str3 = "hi";
    char* str4 = "other string";

    // Unknown since the there may be multiple copies of the same string
    __goblint_check(str != str2); // UNKNOWN!

    __goblint_check(str == str);
    __goblint_check(str != str3);

    char *ptr = NULL;
    int top = rand();

    if(top){
        ptr = str2;
    } else {
        ptr = str3;
    }
    __goblint_check(ptr == str); //UNKNOWN!

    __goblint_check(ptr != str4);

    char *ptr2 = unknown_function();

    __goblint_check(ptr2 == str); //UNKNOWN!
    __goblint_check(ptr2 == ptr); //UNKNOWN!

    return 0;
}

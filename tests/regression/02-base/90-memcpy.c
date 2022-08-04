
// Test case taken from sqlite3.c
#include <string.h>

typedef unsigned long u64;

# define EXP754 (((u64)0x7ff)<<52)
# define MAN754 ((((u64)1)<<52)-1)
# define IsNaN(X) (((X)&EXP754)==EXP754 && ((X)&MAN754)!=0)


static int sqlite3IsNaN(double x){
  int rc;   /* The value return */
  u64 y;
  memcpy(&y,&x,sizeof(y));  // Goblint used to crash here
  rc = IsNaN(y);
  return rc;
}

int foo(){
    int x = 23;
    int y;

    memcpy(&y, &x, sizeof(int));

    __goblint_check(y == 23);
    return 0;
}

int bar(){
    int arr[10];
    double y;

    for(int i = 0; i < 10; i++){
        arr[i] = 0;
    }
    __goblint_check(arr[0] == 0);
    __goblint_check(arr[3] == 0);

    memcpy(&arr, &y, sizeof(double));

    __goblint_check(arr[0] == 0); //UNKNOWN!
    __goblint_check(arr[3] == 0); //UNKNOWN

    return 0;
}

int main(){
    sqlite3IsNaN(23.0);
    foo();
    bar();
    return 0;
}

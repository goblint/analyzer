// SKIP PARAM: --set ana.activated[+] lin2vareq

#include <stdio.h>
#define LEN 32
#define add(x,y) (x+y)
#define multiply(x,y) ((x) * (y))
#define subtract(x,y) ((x) - (y))

int main(){
    int z = 32;
    int a = 10;
    int b = 20;
    int result = add(a,b);
    int result2 = multiply(a,b);
    int result3 = subtract(a,b);

    __goblint_check(z == LEN); // SUCCESS
    __goblint_check(result == 30); // SUCCESS
    __goblint_check(result2 == 200); // SUCCESS
    __goblint_check(result3 == -10); // SUCCESS

    return 0;
}

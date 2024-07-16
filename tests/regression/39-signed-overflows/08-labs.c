//PARAM: --enable ana.int.interval --set ana.activated[+] tmpSpecial
#include<stdlib.h>
int main() {
    long data;
    if (data > (-0xffffffff - 1))
    {
        if (labs(data) < 100)
        {
            __goblint_check(data < 100);
            __goblint_check(-100 < data);
            int result = data * data; //NOWARN
        }

        if(labs(data) <= 100)
        {
            __goblint_check(data <= 100);
            __goblint_check(-100 <= data);
            int result = data * data; //NOWARN
        }
    }
    return 8;
}

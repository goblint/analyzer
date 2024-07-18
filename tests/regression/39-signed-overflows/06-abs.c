//PARAM: --enable ana.int.interval --set ana.activated[+] tmpSpecial
#include<math.h>
int main() {
    int data;
    if (data > (-0x7fffffff - 1))
    {
        if (abs(data) < 100)
        {
            __goblint_check(data < 100);
            __goblint_check(-100 < data);
            int result = data * data; //NOWARN
        }

        if(abs(data) <= 100)
        {
            __goblint_check(data <= 100);
            __goblint_check(-100 <= data);
            int result = data * data; //NOWARN
        }

        if(abs(data) - 1 <= 99)
        {
            __goblint_check(data <= 100);
            __goblint_check(-100 <= data);
            int result = data * data; //NOWARN
        }
    }
    return 8;
}
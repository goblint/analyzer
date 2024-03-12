//PARAM: --enable ana.int.interval --enable ana.float.interval --set ana.activated[+] tmpSpecial
#include<stdlib.h>
#include<math.h>

int main() {
    int data;
    if (data > (-0x7fffffff - 1) && llabs(data) < (long)sqrt((double)0x7fffffff))
    {
        int result = data * data; //NOWARN
    }
    return 8;
}

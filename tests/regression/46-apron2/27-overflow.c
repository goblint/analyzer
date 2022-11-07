// SKIP PARAM: --enable ana.int.interval --set ana.activated[+] apron
#include<stdio.h>
typedef long long int64_t;

int main(int argc, char * argv[])
{
    long long data;
    if (data < 0x7fffffffffffffffLL)
    {
        long long result = data + 1; //NOWARN
    }

    return 0;
}
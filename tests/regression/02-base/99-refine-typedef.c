// PARAM: --enable ana.int.interval
typedef signed long int __int64_t;
typedef __int64_t int64_t;

int main(int argc, char * argv[])
{
    // Those two should behave identically, as the typedef should be unrolled

    int64_t data;
    if (data < 100LL)
    {
        __goblint_check(data < 100);
        int64_t result = data + 1; //NOWARN
    }

    signed long int data2;
    if(data2 < 100LL)
    {
        __goblint_check(data2 < 100);
        signed long int result2 = data2 + 1; //NOWARN
    }

    return 0;
}
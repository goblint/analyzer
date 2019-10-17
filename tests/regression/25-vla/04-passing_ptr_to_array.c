//PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.trier --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --set ana.activated "['base','expRelation']"

void foo(int (*a)[40]){
    int x = (*(a + 29))[7];
    assert(x == 23); //UNKNOWN

    int y = (*(a + 7))[13];
    assert(y == 23);

    assert(a[7][13] == 23);
}

int main(void)
{
    int n =40;
    int b[n][n];
    b[7][13] = 23;

    foo(b);
}

// PARAM: --sets solver td3 --enable ana.int.interval --enable exp.partition-arrays.enabled  --set ana.activated "['base','threadid','threadflag','expRelation','mallocWrapper','var_eq']"
int global;

int main(void)
{
    example1();
    example2();
    example3();
    example4();
    example5();
    example6();
    example7();
    example8();
    example9();
    example10();
    return 0;
}

// Simple example
void example1(void)
{
    int top;
    int top2;
    int arr[10];

    arr[top] = 42;
    top2 = top;
    assert(arr[top2] == 42);
}

// PARAM: --set ana.activated[+] memOutOfBounds
// TODO: I haven't checked why, but we need memOutOfBounds for this case
extern int printf ( const char * format, ... );

int *foo2(void)
{
    int arr[1024];
    arr[194] = 13;
    return arr + 1;
}

int *foo(void)
{
    int arr[123];
    return foo2();
}

int main(void) {
    int *a = foo();
    printf("%d\n", *a);//WARN
    return 0;
}

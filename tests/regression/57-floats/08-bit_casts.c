// PARAM: --enable ana.float.interval

typedef union
{
    float value;
    unsigned int word;
} A;

int main()
{
    A a;
    a.word = 3212836864;
    float b = a.value;

    assert(b == -1.0f); // UNKNOWN!

    A a2;
    a2.value = -1.0f;
    unsigned int b2 = a2.word;

    assert(b2 == 1.0f); // UNKNOWN!

    int x = 100;
    float y = *(float *)(&a);
    assert(y == 100.f); // UNKNOWN!

    double i = 100.0;
    unsigned j = *(unsigned *)(&i);
    assert(j == 100); // UNKNOWN!
    return 0;
}

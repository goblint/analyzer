// PARAM: --enable ana.float.interval

void main()
{
    int check1 = (0.2f == 0.2); // WARN
    int check2 = (0.2 != 0.3l); // WARN

    // Not all integers that are this big are representable in doubles anymore...
    double high_value = 179769313486231568384.0;
    double x = high_value + 1; // WARN
}

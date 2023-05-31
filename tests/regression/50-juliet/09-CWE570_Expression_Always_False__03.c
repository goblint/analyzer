void main()
{
    int intTen = 10;

    if (intTen == (intTen-1)) // WARN: expression is always false
    {
        printf("Never prints");
    }
}

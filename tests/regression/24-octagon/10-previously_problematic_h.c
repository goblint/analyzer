// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.def_exc --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --set ana.activated "['base','expRelation','octagon']"
// These examples were cases were we saw issues of not reaching a fixpoint during development of the octagon domain. Since those issues might
// resurface, these tests without asserts are included
int main(int argc, char const *argv[])
{
    int iter = 0;
    int i = 0;
    int j;
    int t;
    int nGroups;
    int inUse16[16];
    int top;

    if (top) {
        nGroups = 2;
    } else {
        nGroups = 6;
    }

    while (iter < 4)
    {
        t = 0;
        while (t < nGroups)
        {
            t++;
        }

        iter++;
    }

    i = 0;
    while (i < 16)
    { //TODO: here
        inUse16[i] = 0;

        j = 0;
        while (j < 16)
        {
            j++;
        }

        i++;
    }

    /* code */
    return 0;
}

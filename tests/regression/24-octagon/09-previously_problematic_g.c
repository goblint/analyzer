// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.def_exc --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --set ana.activated "['base','expRelation','octagon']"
// These examples were cases were we saw issues of not reaching a fixpoint during development of the octagon domain. Since those issues might
// resurface, these tests without asserts are included
int main(int argc, char const *argv[])
{
    int i = 0;
    int j;
    int nGroups = 6;
    int inUse16[16];

    while (i < 16)
    { // TODO: here
        inUse16[i] = 0;
        j = 0;
        i++;
    }

    return 0;
}

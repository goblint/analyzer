// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.def_exc --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --set ana.activated "['base','expRelation','octagon']"
// These examples were cases were we saw issues of not reaching a fixpoint during development of the octagon domain. Since those issues might
// resurface, these tests without asserts are included
int main(int argc, char const *argv[])
{
    int top;

    int pad = 0;
    int to_uppcase;
    int change_case = 0;

    while (change_case != 1 && to_uppcase != 0) {
        if(top == 1) {
            to_uppcase = 1;
            continue;
        }

        if(top == 2) {
            change_case = 1;
            continue;
        }

        break;
    }

    return 0;
}

// PARAM: --enable ana.context.ctx_gas --enable ana.sv-comp.functions --enable ana.float.interval --enable ana.int.interval --enable ana.autotune.enabled --set ana.base.arrays.domain partitioned  --set ana.activated[+] expRelation
/*
 * Date: 07/07/2015
 * Created by: Ton Chanh Le (chanhle@comp.nus.edu.sg)
 */

extern int __VERIFIER_nondet_int();

int f(int x);
int g(int x);

int f(int x)
{
    if (x <= 0)
        return 0;
    else
        return g(x) + g(x + 1);
}

int g(int x)
{
    if (x <= 0)
        return 0;
    else
        return f(x - 1) + f(x - 2);
}

int main()
{
    int x = __VERIFIER_nondet_int();
    int res = g(x);
    __goblint_check(res == 0); // UNKNOWN
}
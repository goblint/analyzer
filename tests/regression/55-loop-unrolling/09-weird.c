// PARAM: --set exp.unrolling-factor 2 --enable dbg.run_cil_check

void main(void)
{
    int j = 0;

    for(int i=0;i < 50;i++) {
        goto somelab;
        somelab: j = 8;
    }

    __goblint_check(j==8);
}

// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --disable ana.int.interval
// copied & modified from 36-apron/38-branch global: made refinable count also global
int ARR_SIZE = 10;
int count = 0;

int main() {
    while(count<ARR_SIZE) {
        __goblint_check(count >= 0);
        __goblint_check(count < ARR_SIZE);
        count++;
    }
    __goblint_check(count == ARR_SIZE); // TODO (requires threshold)
    return 0 ;
}

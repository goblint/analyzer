// PARAM: --set ana.malloc.unique_address_count 1 --set ana.autotune.activated '["reduceAnalyses"]' --enable ana.autotune.enabled
#include<pthread.h>
#include<goblint.h>

int main(int argc, char *argv[]) {
    int* ptr = (int*)malloc(sizeof(int));
    *ptr = 8;
    *ptr = 5;

    __goblint_check(*ptr ==5);

    return 0;
}

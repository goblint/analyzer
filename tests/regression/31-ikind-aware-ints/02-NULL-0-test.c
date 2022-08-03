#include<assert.h>
#include<stdlib.h>
int main(){
    int *ptr = NULL;
    int null = 0;
    int a = 0;
    if((unsigned long )null == (unsigned long ) ptr){
        a++;
    }
    __goblint_check(a == 1);
    return 0;
}

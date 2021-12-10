#include<stdio.h>
struct __anonstruct_mbox_t_232 {
   int one;
   int two;
};
typedef struct __anonstruct_mbox_t_232 mbox_t;

int main(void) {
    unsigned char raw_mbox[15U];
    mbox_t *mbox ;

    mbox = (mbox_t *)(& raw_mbox);

    memset((void *)(& mbox->one), 0, 8UL);
}

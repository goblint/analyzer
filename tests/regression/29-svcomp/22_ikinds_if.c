// PARAM: --conf=./conf/svcomp21.json  --sets ana.specification ./tests/sv-comp/unreach-call-__VERIFIER_error.prp
#include<stdlib.h>

static long sound_ioctl(unsigned int cmd , unsigned long arg )
{
  int len ;
  int dtype ;
  int dev ;
  unsigned int tmp ;
  long ret ;
  void *p ;
  unsigned long flag ;
  unsigned long roksum ;
  struct thread_info *tmp___0 ;
  long tmp___1 ;
  unsigned long flag___0 ;
  unsigned long roksum___0 ;
  struct thread_info *tmp___2 ;
  long tmp___3 ;
  int __pu_err ;
  int tmp___4 ;
  int tmp___5 ;
  int tmp___6 ;
  int tmp___7 ;
  int tmp___8 ;
  int tmp___9 ;
  int tmp___10 ;
  int tmp___11 ;

  {
  len = 0;
  p = (void *)arg;
  if (cmd >> 30 != 0U && cmd >> 30 != 0U) {
    len = (int )(cmd >> 16) & 16383;
    // if ((len <= 0 || len > 65536) || (unsigned long )p == (unsigned long )((void *)0)) {
    if ( (unsigned long )p == (unsigned long )((void *)0)) {
      return (-14L);
    } else {
      return 0;
    }
  }
  }
}

int main(){
    sound_ioctl(2, 0);

    int len;
    unsigned long value;
    void *p = &value;
    if ((unsigned long )p == (unsigned long )((void *)0)) {
      return 1;
    }
    return 0;
}

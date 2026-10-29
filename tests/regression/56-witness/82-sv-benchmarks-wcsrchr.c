#include <stddef.h>
#include <stdlib.h>
#include <alloca.h>

#define __unlikely(x) (x)
#define __likely(x) (x)
#define wchar_t int

extern int __VERIFIER_nondet_int(void);

wchar_t* wcsrchr(const wchar_t *wcs, wchar_t wc) {
  wchar_t* last=0;
  for (; *wcs; ++wcs)
    if (*wcs == wc)
      last=(wchar_t*)wcs;
  return last;
}


int main(){
  int in_len = __VERIFIER_nondet_int();
  if(in_len < 1){return 1;}
  wchar_t* hay = alloca(in_len*sizeof(wchar_t));
  hay[in_len-1]=0;
  wchar_t needle = __VERIFIER_nondet_int();
  return (int) wcsrchr(hay, needle);
}

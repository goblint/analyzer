#include <stddef.h>
#include <stdlib.h>
#include <alloca.h>

#define __unlikely(x) x
#define __likely(x) x


extern int __VERIFIER_nondet_int(void);

size_t strlen(const char *s) {
  register size_t i;
  if (__unlikely(!s)) return 0;
  for (i=0; __likely(*s); ++s) ++i;
  return i;
}

int main(){
  int in_len = __VERIFIER_nondet_int();
  if(in_len < 1){return 1;}
  char* hay = alloca(in_len);
  hay[in_len-1]=0;
  return strlen(hay);
}

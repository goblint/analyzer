#include <stddef.h>
#include <stdlib.h>
#include <alloca.h>

#define __unlikely(x) x
#define __likely(x) x


extern int __VERIFIER_nondet_int(void);

size_t strspn(const char *s, const char *accept)
{
  size_t l = 0;
  const char *a;

  for (; *s; s++) {
    for (a = accept; *a && *s != *a; a++);

    if (!*a)
      break;
    else
     l++;
  }

  return l;
}


int main(){
  int in_len = __VERIFIER_nondet_int();
  if(in_len < 1){return 1;}
  char* hay = alloca(in_len);
  hay[in_len-1]=0;
  int out_len = __VERIFIER_nondet_int();
  if(out_len < 1){return 1;}
  char* needle = alloca(out_len);
  needle[out_len-1]=0;
  return (int) strspn(hay, needle);
}

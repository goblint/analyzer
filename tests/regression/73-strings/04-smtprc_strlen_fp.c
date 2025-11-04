// FIXPOINT extracted from smtprc_comb
#include <unistd.h> // for optarg

typedef unsigned int size_t; // size_t from 32bit cilly
extern size_t strlen(char const   *__s );

void *s_malloc(unsigned long size)
{
  void *mymem;
  mymem = malloc((unsigned int) size);
  return mymem;
}

int main() {
  char const *p;
  size_t s;
  p = optarg;
  s = strlen(optarg);
  s_malloc((unsigned long) ((s + 1U) * sizeof(char)));
  return 0;
}

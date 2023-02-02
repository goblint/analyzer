//PARAM: --set ana.activated[+] tmpSpecial
#include <float.h>
#include <goblint.h>


void main() {
  float f;
  float g;
  float common;
  float* fptr;
  float* gptr;
  int unk1;
  int unk2;

  float other;

  if (unk1)
    fptr = &f;
  else
    fptr = &common;

  if (unk2)
    gptr = &g;
  else
    gptr = &common;


  

  other = __builtin_cos(*fptr);
  *gptr = 0.7;

  __builtin_inf();
}
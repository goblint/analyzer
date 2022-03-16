// PARAM: --set exp.earlyglobs true --set exp.precious_globs[+] "'g'"
#include <assert.h>

int g = 10;
int main(void){
  g = 100;
  assert(g==100);
  return 0;
}
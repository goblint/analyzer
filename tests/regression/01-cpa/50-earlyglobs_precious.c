// PARAM: --set exp.earlyglobs true --set exp.exclude_from_earlyglobs[+] "'g'"
#include <assert.h>

int g = 10;
int main(void){
  g = 100;
  assert(g==100);
  return 0;
}

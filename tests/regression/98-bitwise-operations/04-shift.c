// PARAM: --enable ana.int.interval
#include <goblint.h>

int main() {
  unsigned int x;

  if (x > 10) {
    x = 10;
  }

  unsigned int u_res = x >> 0;

  __goblint_check(u_res >= 0);
  __goblint_check(u_res <= 10); 

  u_res = x >> 1;

  __goblint_check(u_res >= 0);
  __goblint_check(u_res <= 5); 

  u_res = x >> 2;

  __goblint_check(u_res >= 0);
  __goblint_check(u_res <= 2); 

  int x;

  if (x < 0) {
    x = 0;
  }

  int res = x >> 0;

  __goblint_check(res >= 0);
  __goblint_check(res <= __INT_MAX__);


  res = x >> 1;

  __goblint_check(res >= 0);
  __goblint_check(res <= (__INT_MAX__ / 2));


  long long ll_x = __LONG_LONG_MAX__;
  long long ll_res = ll_x >> 0;

  __goblint_check(ll_res >= 0);
  __goblint_check(ll_res <= __LONG_LONG_MAX__);


  ll_res = ll_x >> 2;

  __goblint_check(ll_res >= 0);
  __goblint_check(ll_res <= (__LONG_LONG_MAX__ / 4));
  return 0;
}
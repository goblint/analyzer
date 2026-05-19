// PARAM: --enable ana.int.interval
#include <goblint.h>

int main() {
  int res;
  int pos_x;

  if (pos_x < 0 || pos_x > 30) {
    pos_x = 0;
  }

  res = ~pos_x;
  __goblint_check(res <= 0);
  __goblint_check(res >= -31);

  int pos_x2;

  if (pos_x2 < 20 || pos_x2 > 70) {
    pos_x2 = 70;
  }

  res = ~pos_x2;
  __goblint_check(res <= -21);
  __goblint_check(res >= -71);

  int neg_x;

  if (neg_x < -30 || neg_x > -1) {
    neg_x = -1;
  }

  res = ~neg_x;
  __goblint_check(res >= 0);
  __goblint_check(res <= 29);

  int neg_x2;

  if (neg_x2 < -70 || neg_x2 > -20) {
    neg_x2 = -70;
  }

  res = ~neg_x2;
  __goblint_check(res <= 69);
  __goblint_check(res >= 19);

  int other_x;

  if (other_x < -63 || other_x > 20) {
    other_x = 0;
  }

  res = ~other_x;
  __goblint_check(res <= 62);
  __goblint_check(res >= -21);

  int other_x2;

  if (other_x2 < -1 || other_x2 > 32) {
    other_x2 = 0;
  }

  res = ~other_x2;
  __goblint_check(res >= -33);
  __goblint_check(res <= 0);

  long long ll_res;
  long long ll_large_x;

  if (ll_large_x < -__LONG_LONG_MAX__ + 100 || ll_large_x > __LONG_LONG_MAX__ - 100) {
    ll_large_x = 0;
  }

  ll_res = ~ll_large_x;
  __goblint_check(ll_res <= __LONG_LONG_MAX__ - 101);
  __goblint_check(ll_res >= -__LONG_LONG_MAX__ + 99);

  long long ll_max_x;

  ll_res = ~ll_max_x;
  __goblint_check(ll_res <= __LONG_LONG_MAX__);
  __goblint_check(ll_res >= -__LONG_LONG_MAX__ - 1);

  return 0;
}
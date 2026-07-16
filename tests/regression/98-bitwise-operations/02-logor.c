// PARAM: --enable ana.int.interval
#include <goblint.h>

int main() {
  unsigned int u_res;
  unsigned int unsigned_x;
  unsigned int unsigned_y;

  if (unsigned_x < 14 ) {
    unsigned_x = 14;
  }

  if (unsigned_y < 35 || unsigned_y > 60) {
    unsigned_y = 60;
  }

  u_res = unsigned_x | unsigned_y;
  __goblint_check(u_res >= 35);
  __goblint_check(u_res <= __UINT32_MAX__);
  
  int res;
  int both_pos_x;
  int both_pos_y;

  if (both_pos_x < 0 ||both_pos_x > 64) {
    both_pos_x = 0;
  }

  if (both_pos_y < 10 || both_pos_y > 30) {
    both_pos_y = 30;
  }

  res = both_pos_x | both_pos_y;
  __goblint_check(res >= 10);
  __goblint_check(res <= 127);

  int both_neg_x;
  int both_neg_y;

  if (both_neg_x < -64 || both_neg_x > -1) {
    both_neg_x = -64;
  }

  if (both_neg_y < -30 || both_neg_y > -1) {
    both_neg_y = -30;
  }

  res = both_neg_x | both_neg_y;
  __goblint_check(res <= 0);
  __goblint_check(res >= -30);

  int one_neg_x;
  int one_neg_y;

  if (one_neg_x < -64 || one_neg_x > -1) {
    one_neg_x = -64;
  }

  if (one_neg_y > 63 || one_neg_y < 0) {
    one_neg_y = 63;
  }

  res = one_neg_x | one_neg_y;
  __goblint_check(res <= 0);
  __goblint_check(res >= -64);

  int one_neg_x2;
  int one_neg_y2;

  if (one_neg_x2 < -20 || one_neg_x2 > -1) {
    one_neg_x2 = -20;
  }

  if (one_neg_y2 > 30 || one_neg_y2 < 0) {
    one_neg_y2 = 30;
  }

  res = one_neg_x2 | one_neg_y2;
  __goblint_check(res <= 0);
  __goblint_check(res >= -20); 

  int one_neg_x3;
  int one_neg_y3;

  if (one_neg_x3 < -10 || one_neg_x3 > -1) {
    one_neg_x3 = -10;
  }

  if (one_neg_y3 > 64 || one_neg_y3 < -64) {
    one_neg_y3 = 64;
  }

  res = one_neg_x3 | one_neg_y3;
  __goblint_check(res <= 0);
  __goblint_check(res >= -10);

  int one_neg_x4;
  int one_neg_y4;

  if (one_neg_x4 < -30 || one_neg_x4 > -1) {
    one_neg_x4 = -30;
  }

  if (one_neg_y4 > 10 || one_neg_y4 < -10) {
    one_neg_y4 = 10;
  }

  res = one_neg_x4 | one_neg_y4;
  __goblint_check(res <= 0);
  __goblint_check(res >= -30);

  int otherwise_x;
  int otherwise_y;

  if (otherwise_x < -63 || otherwise_x > 20) {
    otherwise_x = 0;
  }

  if (otherwise_y < -10 || otherwise_y > 63) {
    otherwise_y = 0;
  }
  res = otherwise_x | otherwise_y;
  __goblint_check(res >= -63);
  __goblint_check(res <= 63);

  int x_otherwise2;
  int y_otherwise2;

  if (x_otherwise2 < -64 || x_otherwise2 > 30) {
    x_otherwise2 = 0;
  }

  if (y_otherwise2 < -20 || y_otherwise2 > 64) {
    y_otherwise2 = 0;
  }

  res = x_otherwise2 | y_otherwise2;
  __goblint_check(res >= -64);
  __goblint_check(res <= 127);

  int x_otherwise3;
  int y_otherwise3;

  if (x_otherwise3 < -1 || x_otherwise3 > 1) {
    x_otherwise3 = 0;
  }

  if (y_otherwise3 < -1 || y_otherwise3 > 1) {
    y_otherwise3 = 0;
  }

  res = x_otherwise3 | y_otherwise3;
  __goblint_check(res >= -1);
  __goblint_check(res <= 1);


  long long ll_res;
  long long ll_int_x;
  long long ll_int_y;

  if (ll_int_x < -__INT_MAX__ - 1 || ll_int_x > __INT_MAX__ ) {
    ll_int_x = 0;
  }

  if (ll_int_y < -__INT_MAX__ - 1 || ll_int_y > __INT_MAX__ ) {
    ll_int_y = 0;
  }

  ll_res = ll_int_x | ll_int_y;
  __goblint_check(ll_res >= -__INT_MAX__ - 1);
  __goblint_check(ll_res <= __INT_MAX__);

  long long ll_large_x;
  long long ll_large_y;

  if (ll_large_x < -__LONG_LONG_MAX__ + 100 || ll_large_x > __LONG_LONG_MAX__ - 100) {
    ll_large_x = 0;
  }

  if (ll_large_y < -100 || ll_large_y > 100) {
    ll_large_y = 0;
  }

  ll_res = ll_large_x | ll_large_y;
  __goblint_check(ll_res >= -__LONG_LONG_MAX__ - 1);
  __goblint_check(ll_res <= __LONG_LONG_MAX__);

  long long ll_max_x;
  long long ll_max_y;

  if (ll_max_y < -100 || ll_max_y > 100) {
    ll_max_y = 0;
  }

  ll_res = ll_max_x | ll_max_y;
  __goblint_check(ll_res >= -__LONG_LONG_MAX__ - 1);
  __goblint_check(ll_res <= __LONG_LONG_MAX__);

  return 0;
}
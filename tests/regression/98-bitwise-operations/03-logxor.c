// PARAM: --enable ana.int.interval
#include <goblint.h>

int main() {
  
  int zero_x = 0;
  int zero_y = 0;

  __goblint_check((zero_x ^ zero_y) >= 0);
  __goblint_check((zero_x ^ zero_y) <= 0);
  
  int both_pos_x;
  int both_pos_y;

  if (both_pos_x < 0 ||both_pos_x > 63) {
    both_pos_x = 50;
  }

  if (both_pos_y < 0 || both_pos_y > 30) {
    both_pos_y = 32;
  }
  __goblint_check((both_pos_x ^ both_pos_y) >= 0);
  __goblint_check((both_pos_x ^ both_pos_y) <= 63);

  int both_pos_x2;
  int both_pos_y2;
  if (both_pos_x2 < 0 || both_pos_x2 > 1) {
    both_pos_x2 = 0;
  }

  if (both_pos_y2 < 0 || both_pos_y2 > 1) {
    both_pos_y2 = 0;
  }

  __goblint_check((both_pos_x2 ^ both_pos_y2) >= 0);
  __goblint_check((both_pos_x2 ^ both_pos_y2) <= 1);

  int both_neg_x;
  int both_neg_y;

  if (both_neg_x < -64 || both_neg_x > -1) {
    both_neg_x = -64;
  }

  if (both_neg_y < -64 || both_neg_y > -1) {
    both_neg_y = -64;
  }

  __goblint_check((both_neg_x ^ both_neg_y) >= 0);
  __goblint_check((both_neg_x ^ both_neg_y) <= 63);

  int both_neg_x2 = -1;
  int both_neg_y2 = -1;

  __goblint_check((both_neg_x2 ^ both_neg_y2) >= 0);
  __goblint_check((both_neg_x2 ^ both_neg_y2) <= 0);

  int neg_pos_x;
  int neg_pos_y;

  if (neg_pos_x < -64 || neg_pos_x > -1) {
    neg_pos_x = -64;
  }

  if (neg_pos_y > 64 || neg_pos_y < 0) {
    neg_pos_y = 64;
  }

  __goblint_check((neg_pos_x ^ neg_pos_y) <= 0);
  __goblint_check((neg_pos_x ^ neg_pos_y) >= -64);

  int neg_pos_x2;
  int neg_pos_y2;

  if (neg_pos_x2 < -65 || neg_pos_x2 > -1) {
    neg_pos_x2 = -65;
  }

  if (neg_pos_y2 > 65 || neg_pos_y2 < 0) {
    neg_pos_y2 = 65;
  }

  __goblint_check((neg_pos_x2 ^ neg_pos_y2) <= 0);
  __goblint_check((neg_pos_x2 ^ neg_pos_y2) >= -128);  

  int neg_pos_x3 = -1;
  int neg_pos_y3;

  if (neg_pos_y3 > 1 || neg_pos_y3 < 0) { 
    neg_pos_y3 = 1;
  }

  __goblint_check((neg_pos_x3 ^ neg_pos_y3) <= 0);
  __goblint_check((neg_pos_x3 ^ neg_pos_y3) >= -2);

  int otherwise_x;
  int otherwise_y;

  if (otherwise_x < -63 || otherwise_x > 20) {
    otherwise_x = 0;
  }

  if (otherwise_y < -10 || otherwise_y > 63) {
    otherwise_y = 0;
  }

  __goblint_check((otherwise_x ^ otherwise_y) >= -64);
  __goblint_check((otherwise_x ^ otherwise_y) <= 63);

  int x_otherwise2;
  int y_otherwise2;

  if (x_otherwise2 < -64 || x_otherwise2 > 30) {
    x_otherwise2 = 0;
  }

  if (y_otherwise2 < -20 || y_otherwise2 > 64) {
    y_otherwise2 = 0;
  }

  __goblint_check((x_otherwise2 ^ y_otherwise2) >= -128);
  __goblint_check((x_otherwise2 ^ y_otherwise2) <= 127);

  int x_otherwise3;
  int y_otherwise3;

  if (x_otherwise3 < -1 || x_otherwise3 > 1) {
    x_otherwise3 = 0;
  }

  if (y_otherwise3 < -1 || y_otherwise3 > 1) {
    y_otherwise3 = 0;
  }

  __goblint_check((x_otherwise3 ^ y_otherwise3) >= -2);
  __goblint_check((x_otherwise3 ^ y_otherwise3) <= 1);


  long long ll_large_x;
  long long ll_large_y;

  if (ll_large_x < -__LONG_LONG_MAX__ + 100 || ll_large_x > __LONG_LONG_MAX__ - 100) {
    ll_large_x = 0;
  }

  if (ll_large_y < -100 || ll_large_y > 100) {
    ll_large_y = 0;
  }

  __goblint_check((ll_large_x ^ ll_large_y) >= -__LONG_LONG_MAX__ - 1);
  __goblint_check((ll_large_x ^ ll_large_y) <= __LONG_LONG_MAX__);

  long long ll_max_x;
  long long ll_max_y;

  if (ll_max_y < -100 || ll_max_y > 100) {
    ll_max_y = 0;
  }

  __goblint_check((ll_max_x ^ ll_max_y) >= -__LONG_LONG_MAX__ - 1);
  __goblint_check((ll_max_x ^ ll_max_y) <= __LONG_LONG_MAX__);
  return 0;
}

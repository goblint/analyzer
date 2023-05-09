// SKIP: this is an input file for cram tests

int f_both(int x) {
  int result;

  if (x > 7)
    goto true_block;
  else
    goto false_block;

  // condition never checked, but true and false blocks live
  if (x * 12 > 3) {
    true_block:
    result = 2;
  } else {
    false_block:
    result = 12;
  }

  return result;
}

int f_true(int x) {
  int result;

  if (x > 7)
    goto true_block;
  else
    goto false_block;

  // condition never checked, and only true block live
  if (x * 12 > 3) {
    true_block:
    result = 2;
  } else {
    false_block:
    result = 12;
  }

  return result;
}

int f_false(int x) {
  int result;

  if (x > 7)
    goto true_block;
  else
    goto false_block;

  // condition never checked, and only false block live
  if (x * 12 > 3) {
    true_block:
    result = 2;
  } else {
    false_block:
    result = 12;
  }

  return result;
}

int main() {
  f_both(3);
  f_both(9);

  f_true(12);
  f_false(-3);
}

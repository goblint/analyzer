// PARAM: --set solver td3 --enable ana.int.interval  --enable ana.base.partition-arrays.enabled --set ana.base.partition-arrays.keep-expr "last"  --set ana.activated "['base','threadid','threadflag','escape','expRelation','mallocWrapper']" --set ana.base.privatization none
int main(void)
{
  int arr[260];
  int n;

  n = 5;
  arr[0] = 0;

  while (n > 1) { //here
    arr[1] = 7;
    n--;
  }

  return 0;
}

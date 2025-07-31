// PARAM: --enable ana.int.interval_set
int count_int_int;
void main() {
  for (;; count_int_int++) // TODO WARN (overflow)
    ;
}

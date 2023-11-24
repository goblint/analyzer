// SKIP TERM PARAM: --enable ana.autotune.enabled --enable ana.sv-comp.functions --enable ana.sv-comp.enabled  --set ana.autotune.activated "['congruence']" --set ana.specification "CHECK( init(main()), LTL(F end) )"

// This task previously crashed due to the autotuner
int main() {
	int a;
    int odd, count = 0;
    while(a > 1) {
      odd = a % 2;
      if(!odd) a = a / 2;
      else a = a - 1;
      count++;
    }
    return count;
}

// PARAM: --enable ana.int.interval
// Adapted from sv-comp array-programs/partial_mod_count_1.c
int N = 1000;
int main(){
  int i;

  for(i=0;i<N;i++){
    int z = 5;
    if(i>N/2) {
        z++;
    }

  }
  return 0;
}

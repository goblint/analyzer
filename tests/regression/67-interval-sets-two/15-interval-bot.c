// PARAM: --enable ana.int.interval_set --enable ana.int.def_exc

int main(){

  unsigned long long a ;
  unsigned long long addr;

  if(a + addr > 0x0ffffffffULL){
    return 1;
  }
}

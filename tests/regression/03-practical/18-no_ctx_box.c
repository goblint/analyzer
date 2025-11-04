// SKIP PARAM: --set ana.activated[-] mutex --set solver "'new'" --set ana.ctx_insens[+] base --set ana.ctx_insens[+] escape
void write(int **p){
  *p=1;
}

int *P = 0;

void f(){
  int *a;
  write(&a);
  return;
}

int main(void){
  write(&P);
  f();
  return 0; // NOWARN
}

// SKIP PARAM: --no mutex --set solver "'new'" --no-context base --no-context escape
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
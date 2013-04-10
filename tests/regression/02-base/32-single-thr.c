// PARAM: --set exp.single-threaded true
extern int no_spawn(void*);

int g;

void f(){
  g = 20; // NOWARN! 
}

int main(){
  g = 10; // NOWARN!
  no_spawn(&f);
  return 0;
}
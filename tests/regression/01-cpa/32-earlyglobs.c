// PARAM: --set exp.earlyglobs true

int g = 10;
int main(void){
  g = 100;
  assert(g==100); //UNKNOWN!!!
  return 0;
}
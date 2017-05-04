#include <stdio.h>

int g = 1;

int f(int x){
  return x*g*2;
}

int main(){
  int y;
  y = f(1);
  y = f(2);
  g = 2;
  y = f(2);
  return 0;
}
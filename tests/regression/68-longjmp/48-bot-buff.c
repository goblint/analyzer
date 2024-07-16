#include<setjmp.h>
jmp_buf env_buffer;

int main() {
  longjmp(env_buffer, 1); //WARN
}

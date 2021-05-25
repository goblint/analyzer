#include <assert.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/socket.h>

// DO NOT run this code!

void test_scanf() {
  int n = 0, m = 0;
  scanf("%d %d", &n, &m);
  assert(n == 0); // UNKNOWN!
  assert(m == 0); // UNKNOWN!
}

void test_memset() {
  int n = 0;
  memset(&n, 1, sizeof(int));
  assert(n == 0); // UNKNOWN!
}

void test_bzero() {
  int n = 1;
  bzero(&n, sizeof(int));
  assert(n == 1); // UNKNOWN!
}

void test_getopt() {

}

void test_fprintf() {

}

void test_vfprintf() {

}

void test_vprintf() {

}

void test_free() {
  int* n = malloc(sizeof(int));
  *n = 0;
  free(n);
  assert(n != 0);
}

void test_memcpy() {
  int dest = 0;
  int src = 1;
  
  memcpy(&dest, &src, sizeof(int));

  assert(dest == 0); // UNKNOWN!
  assert(src == 1);
}

void test_fopen() {

}

void test_connect() {
  struct sockaddr sa;
  sa.sa_family = 0;
  connect(0, &sa, 0);
  assert(sa.sa_family == 0);
}

void test_sprintf() {

}

void test_vsprintf() {

}

void test_localtime() {
  time_t t = 1;
  localtime(&t);
  assert(t == 1);
}

void test_time() {
  time_t t = 1;
  time(&t);
  assert(t == 1); // UNKNOWN!
}

void test_strftime() {

}

void test_snprintf() {

}

void test_send() {
  int n = 0;
  send(0, &n, sizeof(int), 0);
  assert(n == 0);
}

void test_recv() {
  int n = 0;
  recv(0, &n, sizeof(int), 0);
  assert(n == 0); // UNKNOWN!
}

void test_fwrite() {
  int n = 0;
  fwrite(&n, sizeof(int), 1, stdout);
  assert(n == 0);
}

void test_atoi() {

}

void test_fflush() {

}

void test_strstr() {

}

void test_strlen() {

}

void test_strcmp() {

}

void test_strncpy() {

}

void test_strncmp() {

}

void test_write() {
  int n = 0;
  write(0, &n, sizeof(int));
  assert(n == 0);
}

void test_fclose() {

}

void test_fread() {
  int n = 0;
  fread(&n, sizeof(int), 1, stdin);
  assert(n == 0); // UNKNOWN!
}

void test_read() {
  int n = 0;
  read(0, &n, sizeof(int));
  assert(n == 0); // UNKNOWN!
}

void test_printf() {

}

void test_sscanf() {
  int n = 0, m = 0;
  sscanf("10 20", "%d %d", &n, &m);
  assert(n == 0); // UNKNOWN!
  assert(m == 0); // UNKNOWN!
}

int main () {
  test_atoi();
  test_bzero();
  test_connect();
  test_fclose();
  test_fflush();
  test_fopen();
  test_fprintf();
  test_fread();
  test_free();
  test_fwrite();
  test_getopt();
  test_localtime();
  test_memcpy();
  test_memset();
  test_printf();
  test_read();
  test_recv();
  test_scanf();
  test_send();
  test_snprintf();
  test_sprintf();
  test_sscanf();
  test_strcmp();
  test_strftime();
  test_strlen();
  test_strncmp();
  test_strncpy();
  test_strstr();
  test_time();
  test_vfprintf();
  test_vprintf();
  test_vsprintf();
  test_write();
  assert(0); // FAIL!
  return 0;
}

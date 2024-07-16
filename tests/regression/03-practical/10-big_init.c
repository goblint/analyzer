// Just an example of slow initialization.
typedef unsigned char BYTE;
BYTE Buffer[4096];

typedef char TEXT[20];
typedef TEXT TABLE[20];
TABLE MessageSystem[20];

int main() {
  __goblint_check(1); // reachable, formerly TERM
  return 0;
}

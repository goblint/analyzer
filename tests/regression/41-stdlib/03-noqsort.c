// PARAM: --set pre.cppflags[+] -DGOBLINT_NO_QSORT
#include<stddef.h>

// There should be no CIL warning about multiple definitions here
void qsort(void *ptr, size_t count, size_t size, int (*comp)(const void*, const void*), int more) {
}

int main() {

}

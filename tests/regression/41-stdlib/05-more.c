// PARAM: --set pre.cppflags[+] -DGOBLINT_NO_QSORT
#include<goblint.h>
#include <wchar.h>
#include <stdio.h>

int g = 8;

int main() {

    int r = __builtin_popcount(5u);
    int r = __builtin_popcountl(5ul);
    int r = __builtin_popcountll(5ul);
    puts("blarg");

    int a = strspn("bla","blu");
    a = strcspn("bla","blu");

    long r =  strtol("bla", 0, 8);

    wchar_t wcsbuf[100];
    wchar_t wstring[] = L"ABCDE";
    int     num;

    num = swprintf(wcsbuf, 100, L"%s", "xyz");

    // Should not be invalidated
    __goblint_check(g == 8);
}

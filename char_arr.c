#include <stdio.h>

int main(){

char G66[11][30] =
                          {{(char )'I', (char )'N', (char )'I', (char )'T',
                            (char )'\000'},
                           {(char )'A', (char )'C', (char )'E', (char )'M',
                            (char )'\000'},
                           {(char )'B', (char )'R', (char )'\000'},
                           {(char )'S', (char )'C', (char )'R', (char )'U',
                            (char )'T', (char )'\000'},
                           {(char )'T', (char )'I', (char )'M', (char )'E',
                            (char )'R', (char )'\000'},
                           {(char )'D', (char )'D', (char )'D', (char )'7',
                            (char )'5', (char )'\000'},
                           {(char )'D', (char )'D', (char )'6', (char )'\000'},
                           {(char )'D', (char )'D', (char )'D', (char )'D',
                            (char )'7', (char )'4', (char )'\000'},
                           {(char )'D', (char )'D', (char )'3', (char )'2',
                            (char )'\000'},
                           {(char )'T', (char )'F', (char )'T', (char )'P',
                            (char )'\000'},
                           {(char )'B', (char )'N', (char )'\000'}};

char* a = "Hi"; // CIL: one CStr: a = (char *)"Hi";
char b[] = {"foo"}; // CIL: multiple CChr:  b[0] = (char )'f'; b[1] = ...
char c[10] = {'b','a','r','\000'}; // same CIL as the one above
printf("a: %s, b: %s, c: %s", a, b, c);

extern void foo(char* str);
foo(G66[1]); // use query EvalStr here
}

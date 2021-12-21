extern int *printf(char *, ...);
extern char *strcpy(char *str1, const char *str2);

main () {
  char s[80];
  char t[80];
  char *y;
  strcpy (s, "Hello, strings!\n");
  strcpy (t, "Bye-bye!\n");
  y = strcpy (s, t);
  printf ("%s", y);
}
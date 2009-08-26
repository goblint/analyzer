int ftw(const char *dir, int (*fn)(const char *file, const struct stat *sb, int flag), int nopenfd) {
  fn("", 0, 0);
  return 0;
}

// NOCRASH
extern int strcmp(const char *, const char *);

static int hello_read(const char *path) {
    return strcmp(path, "Hello");
}

static const struct ops {
	int (*read)(const char *);
} hello_oper = {
	.read = hello_read,
};

extern int main_real(const struct ops *);

int main() {
	return main_real(&hello_oper);
}
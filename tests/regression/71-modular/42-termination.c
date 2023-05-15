// PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'"
struct _IO_FILE
{
	char *_IO_read_ptr;
};
struct _IO_FILE freadptrinc_fp;
unsigned long freadptrinc_increment;

void freadptrinc()
{
	freadptrinc_fp._IO_read_ptr += freadptrinc_increment;
}

void freadseek(void)
{
	unsigned long total_buffered;
	while (total_buffered > 0)
	{
		{
			freadptrinc();
		}
	}
}


// see https://github.com/goblint/cil/issues/81

#define __stringify_1(x...) #x
#define __stringify(x...) __stringify_1(x)

#define __ASM_FORM(x, ...) " " __stringify(x, ##__VA_ARGS__) " "
#define __ASM_SEL(a, b) __ASM_FORM(b)

#define _ASM_PTR __ASM_SEL(.long, .quad)
#define _ASM_ALIGN __ASM_SEL(.balign 4, .balign 8)

#define JUMP_TABLE_ENTRY                                      \
	".pushsection __jump_table,  \"aw\" \n\t" _ASM_ALIGN "\n\t" \
	".long 1b - . \n\t"                                         \
	".long %l[l_yes] - . \n\t" _ASM_PTR "%c0 + %c1 - .\n\t"     \
	".popsection \n\t"

#define __stringify_1(x...) #x
#define __stringify(x...) __stringify_1(x)

#define asm_volatile_goto(x...) \
	do                            \
	{                             \
		asm goto(x);                \
		asm("");                    \
	} while (0)

static int arch_static_branch(const int key, const int branch)
{
	asm_volatile_goto("1:"
										".byte " __stringify(BYTES_NOP5) "\n\t" JUMP_TABLE_ENTRY
										:
										: "i"(key), "i"(branch)
										:
										: l_yes);

	return 0;
l_yes:
	return 1;
}

int main()
{
	return 0;
}
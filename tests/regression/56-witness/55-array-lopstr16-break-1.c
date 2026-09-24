// PARAM: --enable ana.sv-comp.enabled --enable ana.sv-comp.functions --enable ana.int.interval --set exp.unrolling-factor 2 --set ana.specification "CHECK( init(main()), LTL(G ! overflow) )" --set ana.activated[+] unassume --set witness.yaml.validate 55-array-lopstr16-break-1.yml --set witness.yaml.unassume 55-array-lopstr16-break-1.yml --enable ana.unassume.precheck

// Copied & modified from sv-benchmarks/c/array-lopstr16/break-1.c.
// The witness for this has been generated with loop unrolling 2.
// Validating the disjunctive invariants (with same loop unrolling) requires unassume precheck.
// In the first invariant, precheck needs to prune impossible top-level disjuncts (for the current unroll).
// In the second invariant, precheck needs to prune impossible deeper disjuncts (for the current unroll).

#include <stdlib.h>
#include <assert.h>

extern int __VERIFIER_nondet_int(void);

#define SIZE 1000000

struct S
{
	int *n;
};

struct S s[SIZE];

int main()
{
	int i;
	int c=__VERIFIER_nondet_int();
	for(i = 0; i < SIZE; i++) // TODO SUCCESS (witness)
	{
		if(c > 5)
			break;

		s[i].n = malloc(sizeof(int));
	}

	for(i = 0; i < SIZE; i++) // SUCCESS (witness)
	{
		if(c <= 5)
			assert(s[i].n != NULL); // UNKNOWN
	}

	return 0;
}

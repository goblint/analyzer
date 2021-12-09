// PARAM: --enable exp.partition-arrays.enabled
int garr[7];

int main(int argc, char **argv)
{
	unsigned char *arr[3];
    unsigned char * top;

	// Call to a function with a missing fundef causes invalidation of garr
	fundef_missing();

	garr[0] = 8;


	if (garr[1] == 1) {
		// Statement so CIL doesn't optimize the if out
		int ret = 12;
	}
}

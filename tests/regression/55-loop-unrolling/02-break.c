int main(void) {
	int r=5;
    for (int i = 0; i < 2; ++i) {
		switch (i) {
		case 0:
			break;
		case 1:
            r = 8;
		}
	}

	return 0;
}

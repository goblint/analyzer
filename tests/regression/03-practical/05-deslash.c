// TERM.
int deslash(unsigned char *str) {
    unsigned char *wp, *rp;

    rp = wp = str;

    while (*rp)
    {
	if (*rp != '\\')
	    *wp++ = *rp++;
	else
	{
	    switch (*++rp)
	    {
	      case 'n':
		*wp++ = 10;
		++rp;
		break;

	      case 'r':
		*wp++ = 13;
		++rp;
		break;

	      case 't':
		*wp++ = 9;
		++rp;
		break;

	      case 'b':
		*wp++ = 8;
		++rp;
		break;

	      case 'x':
		++rp;
		*wp++ = 22; //get_char_code(&rp, 16);
		break;

	      case '0':
		*wp++ = 42; //get_char_code(&rp, 8);
		break;

	      case '1':
	      case '2':
	      case '3':
	      case '4':
	      case '5':
	      case '6':
	      case '7':
	      case '8':
	      case '9':
		*wp++ = 99; // get_char_code(&rp, 10);
		break;

	      default:
		*wp++ = *rp++;
		break;
	    }
	}
    }


    *wp = '\0';

    return wp-str;
}

int main() {
  char *x = "kala";
  deslash(x);
  printf("%s",x);
  return 0;
}


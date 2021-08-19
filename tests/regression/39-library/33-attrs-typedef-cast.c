// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs

typedef int wchar_t;
typedef long size_t;

typedef int inti;

// The following is an extracted and minimized example from musl libc, for which the analysis did not terminate
//#line 3 "src/string/wcstok.c"
wchar_t *wcstok(wchar_t * __restrict  s___0 , wchar_t ** __restrict  p___2 )
{
  if (! s___0) {
    s___0 = (wchar_t * __restrict)*p___2;
    return ((wchar_t *)((void *)0));
  }

  *p___2 = (wchar_t *)(s___0);
  return ((wchar_t *)s___0);
}


int main(){
  wchar_t w = 3;
  wchar_t *wptr = &w;
  wchar_t *wptrptr = &wptr;

  wcstok(wptr, wptrptr);
  return 0;
}

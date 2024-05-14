// PARAM: --set ana.activated[+] abortUnless --enable exp.arg
// NOCRASH

void printLine()
{

}

void goodB2G1Sink(int data)
{
    // abortUnless=true (one int argument, nothing has ruined it yet)
    // sync map: {[abortUnless=true, ...] -> {[abortUnless=true, ...]}}
    // enter: (abortUnless=false, ...) (enter ruined it)
    // left sync map must be updated to: {[abortUnless=false, ...] -> {[abortUnless=true, ...]}}
    printLine();
    // left sync map is looked up by abortUnless=false in combine_env
}

int main()
{
    goodB2G1Sink(0);
    return 0;
}

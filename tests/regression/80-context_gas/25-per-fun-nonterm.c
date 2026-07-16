//PARAM: --enable ana.int.interval_set --set ana.context.gas_value 3 --set ana.context.gas_scope function
// NOTIMEOUT
void h(int n) {
    int x;

    if(x) {
        return;
    }

    g(n+1);
    h(n+1);
}

void g(int n) {
    int x;

    if(x) {
        return;
    }

    g(n+1);
    h(n+1);
}

int main()
{
    g(0);
    h(0);
}

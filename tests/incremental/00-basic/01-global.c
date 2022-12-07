// Previosuly, the function was erroneously not reanalyzed when the global initializer/the start state changed
// when hash-consing is activated.
int g = 0;

int main(){
    int x = g;
    return x;
}

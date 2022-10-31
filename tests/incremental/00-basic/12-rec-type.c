// The incremental used to crash in a stack overflow due in CompareAST
struct s_t ;
typedef struct s_t s_t;

union union_t {
    int i;
    s_t *s[sizeof(s_t *)]; // This caused problems
};

struct s_t {
    int i;
    union union_t u ;
};

int main(){
    s_t n;
    n.i = 23;
    return 0;
}

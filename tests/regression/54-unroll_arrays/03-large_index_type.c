//PARAM: --set ana.base.arrays.domain unroll --set ana.base.arrays.unrolling-factor 5
//from sv-comp test c/aws-c-common/memset_using_uint64_harness.i

typedef long unsigned int size_t;

int main() { 
    int d[];
    size_t num_uint64s;

    for (size_t i = 0; i < num_uint64s; ++i) {
        d[i] = 0;
    }
}

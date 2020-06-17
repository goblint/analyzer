// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.def_exc --disable exp.fast_global_inits  --enable exp.partition-arrays.enabled  --set ana.activated "['base','expRelation']"
struct some_struct
{
    int dir[7];
    int length;
    double coeff;
    double forwback;
};

struct some_struct q_paths[688];
int num_q_paths;

int add_basic_path(int length, double coeff)
{
    int ir[4];
    int j;
    int flag;

    ir[2] = 0;
    while (ir[2] < 2)
    {
        ir[3] = 0;
        while (ir[3] < 2)
        {
            j = 0;
            while (j < num_q_paths)
            {
                if (flag == 1)
                {
                    break;
                }
                j++;
            }

            q_paths[num_q_paths].length = length;

            num_q_paths++;

            (ir[3])++;
        }
        (ir[2])++;
    }

    return 42;
}

int main(int argc, char **argv)
{
    double this_coeff;
    int pl;
    add_basic_path(pl, this_coeff);
}

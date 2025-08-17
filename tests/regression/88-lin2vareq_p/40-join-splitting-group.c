//SKIP PARAM: --set ana.activated[+] lin2vareq_p --set sem.int.signed_overflow assume_none 

int main() {
    //Test if inequality information is conserved correctly when a equality grouping is split
    //Tested with x and y to be resitant against changes to the indexes of variables
    {
        int x, y, z;

        if (z) {
            y = 2 * x;
            if (y < z) 
                ;
            else 
                return 0;
            // y = 2x, 2x <= z - 1
        } else {
            x = 0;
            y = 1;
            z = 10;
        }
        // 2x <= z - 1 would no longer prove what we want, as the relation y = 2x no longer holds
        __goblint_check( y < z); //SUCCESS
    }
    {
        int x, y, z;

        if (z) {
            y = 2 * x;
            if (x < z) 
                ;
            else 
                return 0;
        } else {
            x = 0;
            y = 1;
            z = 10;
        }

        __goblint_check( x < z); //SUCCESS
    }
}
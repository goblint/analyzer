## Notes for the analysis of the Microsoft Research Codebase on SubPolyhedra:

### The Info field:
contains the linear expression upon insertion of the slack variable. Does not get updated with gaussian elimination or row-echelon form but does get updated with renaming of variables (not sure if this happens) and substitution.

### Forget_vars:
When a variable x is 'forgotten' we perform the following steps:
1.  Perform Gaussian Elimination to remove x from as many rows as possible.
    This leaves only one row with non-zero x column.
2.  Delete this last row containing x.
3. 1. If we can find a substitution for x before it is overwritten or unknown, meaning we can express x as a constant or as a combination of other variables we swap all occurences of x in the info fields with the substitution and they can remain valid.
3. 2. If we cannot find this substitution, we remove it from the info field, it becomes a sort of ghost slack variable but the interval still stays valid. So we keep the interval but lose the info. This gets interesting during the join where we try to see if the slack variables in the Polyhedron from one state are the same as the other state. This is done by finding an injective mapping based on the info field. But because our slack variable was removed from the info rows depending on that will probably be dropped.

Later when doing Simplex, whenever there are multiple slack variables in a row it treats them as additional constraints like {0 <= b_1 <= 10} to solve the system and find closer bounds. Slack variables may be consolidated when they are redundant without loss of precision. So in a row like
b1 - b2 = 5 we can substitute b1 in the matrix with b2 + 5 and reduce the dimensionality of the problem.

** Important: The intervals can be valid while variables that helped create this constraint are no longer present. **

** Important: We need constants in the matrix for precision! Because we will have multiple slack variables per constraint we need some other way to identify them from program variables. Then we could have the last column always be the constant one. **

** Important: The Matrix and the Intervals are Rational for maximum precision and because we need rationals for gaussian elimination. Therefore slack variables can have coefficients, these are not propagated into the intervals until the reduction step! **

=> old comment from subpolycore, copied here to organize the code better:
(********************
    1 3 2   -1
    0 5 4       -1

    2 1 0   -2  1
    0 5 4       -1


    FINAL
    2 1 0   -2   1


    2 3 1  4 5 -1
    0 2 5  2 5    -1
    0 0 3  4 6        -1
    2 0 0  5 1           -1
    0 3 0  0 5               -1
    4 0 0 10 2 -1

    2 3 1  4 5 0 -1
    0 2 5  2 5 0   -1
    0 0 3  4 6 0       -1
    0 0 0  5 1 0          -1
    0 0 0  0 5 0              -1
    0 0 0  0 0 0 -1         2
    0 0 0  0 1 2                 -1


    When inserting new constraints we should take care not to insert something linearly dependent on 
    anything already in the constraints. => This is true
    Thus there should be as many rows as there are variables. By this we should be able to derive 
    the point where slack variables start. => This is not true

    Need one function like normalize_affeq that puts constraints into row-echelon form 
    and applies proper changes to intervals. (Normalize slacks to -1, thereby we essentially
    get a protocol matrix which tells us which operations were carried our for normalization.)
    Let's add the function to the domain for now and we can change it later if we realize that
    we need to use it inside the core functionality =>This is not true

***********************)
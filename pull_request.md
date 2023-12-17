This draft PR introduces an analysis for linear two variable equalities (ref: [A. Flexeder, M. Petter, and H. Seidl Fast Interprocedural Linear Two-Variable Equalities](http://doi.acm.org/10.1145/2049706.2049710)). The structure of the analysis is mostly based on the affine equality analysis.

Abstract states in this domain are represented by structs containing an optional array and an apron environment. 
The bottom element is represented by a struct with "None" instead of the array.

The arrays are modeled as proposed in the paper: Each variable is assigned to an index and each array element represents a linear relationship that must hold at the corresponding program point.
The apron environment is used to organize the order of columns and variables.

The length of the array always corresponds to the number of variables in the environment.
If for example in the array at index j we store the element (Some i, k), this means that our analysis found out that x_i = x_j + k. If the array entry at index j is (None, k), it means that x_j = k, where k is a constant and x_i and x_j are variables.

In order to have less code duplication, we moved some functions from affineEqualityDomain to sharedFunctions, such that affineEqualityDomain and our linearTwoVarEqualityDomain can both use them.

This draft currently only supports equalities of the form `x = y + c`. We will extend it to support equalities of the form `a * x = b * y + c` where `x` and `y` are arbitrary variables, `a` and `b` are constants and `c` can be a linear expression of various constants and variables which are equal to a constant i.e. the linear expression must be equal to a constant.
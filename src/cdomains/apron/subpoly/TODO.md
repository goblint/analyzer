## SubPolyhedra Domain TODO


### Lattice Operations
- [ ] **Join** : TODO next!
- [ ] **Meet** : Pairwise meet!
- [ ] **Widening** : Later on!
- [ ] **Leq** : Can be implemented with join.

### Parsing
- [ ] **Implement** bound_texpr: responsible for internal queries, asserts and ?branching?, returns correct (tight) interval bounds for an expression.
- [ ] **Check** Monomoial simplification


### 3. Variable Management
- [ ] **forget_vars**: 
    - [ ] Add function checking if a variable x that is forgotten can be represented as `x = p` where `p` is a polynomial consisting only of program variables. If x can be represented as a constant we want to find another variable that this constant came from to substitute x. The Goal is to replace the info field of slack variables containing x to a valid substitution.
- [ ] **remove redundancies**: Implement a pass to identify and remove redundant slack variables (e.g., $b_1 - b_2 = 5$) to reduce dimensionality.

### 5. Reduction
- [ ] **Simplex** : For now identity will suffice, simplex for more precision later.

### 6. Verification & Testing
- [ ] **Unit Tests**
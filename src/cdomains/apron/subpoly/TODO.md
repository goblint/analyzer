## SubPolyhedra Domain TODO


### Lattice Operations
- [ ] **Join** 
- [ ] **Meet**
- [ ] **Widening**
- [ ] **Leq**

### Parsing
- [ ] **Check**: We currently have lots of AI code in the modules related to parsing `CIL` expressions
- [ ] **Check** Whenever the polyhedron representation changes we need to check whether the information is correctly translated from `CIL` expressions into polyhedron form.

### 3. Variable Management
- [ ] **forget_vars**: 
    - [ ] Add function checking if a variable x that is forgotten can be represented as `x = p` where `p` is a polynomial consisting only of program variables. If x can be represented as a constant we want to find another variable that this constant came from to substitute x. The Goal is to replace the info field of slack variables containing x to a valid substitution.
- [ ] **remove redundancies**: Implement a pass to identify and remove redundant slack variables (e.g., $b_1 - b_2 = 5$) to reduce dimensionality.

### 5. Reduction

### 6. Verification & Testing
- [ ] **Unit Tests**
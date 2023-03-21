# Tests for the support of setjmp/longjmp by Goblint

Some of these tests are technically illegal according to the C standard, as `setjmp` may only appear inside a condition.
We still have it pulled out here sometimes to allow conveniently inspecting analysis results without having to deal with
CIL inserted temporaries. GCC also support this without any issues.

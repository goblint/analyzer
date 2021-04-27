# Step-by-step Instructions to reproduce the Experimental Results


# Outline of how the code is structured

The source code is in the directory src, where the subdirectories are structured as follows:

The most relevant directories are:
- src/solvers: Different fix-point solvers that can be used by Goblint (default is TD3)
- src/domains: Various generic abstract domains: Sets, Maps, ...
- src/cdomains: Abstract domains for C programs (e.g. Integers, Addresses, ...)
- src/analyses: Different analyses supported by Goblint
- src/framework: The code of the analysis framework

Other, not directly relevant, directories:
- src/extract: Related to extracting Promela models from C code
- src/incremental: Related to Incremental Analysis
- src/spec: Related to parsing Specifications for an automata-based analysis of liveness properties
- src/transform: Specify transformations to run based on the analysis results
- src/util: Various utility modules
- src/witness: Related to witness generation


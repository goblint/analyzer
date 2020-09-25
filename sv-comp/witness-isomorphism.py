#!/usr/bin/python3

import sys
import networkx as nx

# def witness_node_match(n1, n2):
#     return True
witness_node_match = nx.algorithms.isomorphism.categorical_node_match(
    ["entry", "sink", "violation", "invariant", "invariant.scope"],
    [False, False, False, None, None]
)

# def witness_edge_match(e1, e2):
#     return True
witness_edge_match = nx.algorithms.isomorphism.categorical_multiedge_match(
    ["assumption", "assumption.scope", "assumption.resultfunction", "control", "startline", "endline", "startoffset", "endoffset", "enterLoopHead", "enterFunction", "returnFromFunction", "threadId", "createThread"],
    [None, None, None, None, None, None, None, None, False, None, None, None, None]
)

expected = nx.read_graphml(sys.argv[1], force_multigraph=True)
actual = nx.read_graphml(sys.argv[2], force_multigraph=True)

isomorphic = nx.is_isomorphic(expected, actual, node_match=witness_node_match, edge_match=witness_edge_match)
print(isomorphic)

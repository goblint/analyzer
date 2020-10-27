#!/usr/bin/python3

import networkx as nx
from pathlib import Path
import sys

def categorical_node_warn(attr, default):
    def warn(data1, data2):
        return [(attr, data1.get(attr, d), data2.get(attr, d)) for attr, d in list(zip(attr, default)) if data1.get(attr, d) != data2.get(attr, d)]

    return warn

def are_isomorphic(path1, path2):
    # def witness_node_match(n1, n2):
    #     return True

    witness_node_match = nx.algorithms.isomorphism.categorical_node_match(
        ["entry", "sink", "violation", "invariant", "invariant.scope"],
        [False, False, False, None, None]
    )
    witness_node_warn = categorical_node_warn([], [])

    # witness_node_match = nx.algorithms.isomorphism.categorical_node_match(
    #     ["entry", "sink", "violation"],
    #     [False, False, False]
    # )
    # witness_node_warn = categorical_node_warn(
    #     ["invariant", "invariant.scope"],
    #     [None, None]
    # )

    # def witness_edge_match(e1, e2):
    #     return True
    witness_edge_match = nx.algorithms.isomorphism.categorical_multiedge_match(
        ["assumption", "assumption.scope", "assumption.resultfunction", "control", "startline", "endline", "startoffset", "endoffset", "enterLoopHead", "enterFunction", "returnFromFunction", "threadId", "createThread"],
        [None, None, None, None, None, None, None, None, False, None, None, None, None]
    )

    expected = nx.read_graphml(path1, force_multigraph=True)
    actual = nx.read_graphml(path2, force_multigraph=True)

    matcher = nx.algorithms.isomorphism.MultiDiGraphMatcher(expected, actual, node_match=witness_node_match, edge_match=witness_edge_match)
    if matcher.is_isomorphic():
        for node1, node2 in matcher.mapping.items():
            warn = witness_node_warn(expected.nodes[node1], actual.nodes[node2])
            for attr, expected_value, actual_value in warn:
                print(f"  {path1} vs {path2}: {attr}: {expected_value} vs {actual_value}")
        return True
    else:
        return False

def check_file(path1, path2):
    isomorphic = are_isomorphic(path1, path2)
    if not isomorphic:
        print(f"{path1} vs {path2}: {isomorphic}")

def check_directory(path1, path2):
    items1 = {path.relative_to(path1) for path in path1.rglob("*") if path.is_file()}
    items2 = {path.relative_to(path2) for path in path2.rglob("*") if path.is_file()}

    items12 = items1 - items2
    items21 = items2 - items1
    if items12:
        print(f"Only in 1: {items12}")
    if items21:
        print(f"Only in 2: {items21}")

    items = items1 & items2
    for item in items:
        check_file(path1 / item, path2 / item)


path1 = Path(sys.argv[1])
path2 = Path(sys.argv[2])
if path1.is_file() and path2.is_file():
    check_file(path1, path2)
else:
    check_directory(path1, path2)
